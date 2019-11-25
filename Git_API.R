#install.packages("jsonlite")
library(jsonlite)
#install.packages("httpuv")
library(httpuv)
#install.packages("httr")
library(httr)

# Can be github, linkedin etc depending on application

oauth_endpoints("github")

# Change based on what you 
myapp <- oauth_app(appname = "Access",
                   key = "aae11990de28cebf5241",
                   secret = "2e27c2f1edf1fcdfce0f75ae890c29dcda4e44da")

# Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

# Use API
gtoken <- config(token = github_token)
req <- GET("https://api.github.com/users/jtleek/repos", gtoken)

# Take action on http error
stop_for_status(req)

# Extract content from a request
json1 = content(req)

# Convert to a data.frame
gitDF = jsonlite::fromJSON(jsonlite::toJSON(json1))

# Subset data.frame
gitDF[gitDF$full_name == "jtleek/datasharing", "created_at"] 






# The code above was sourced from Michael Galarnyk's blog, found at:
# https://towardsdatascience.com/accessing-data-from-github-api-using-r-3633fb62cb08

# -----------------------------------------------------------------------------------
# Interrogate the Github API. R will return the number of followers and public repositories
# in my personal GitHub. I can look at snother persons if I change the username.

myData = GET("https://api.github.com/users/ElizabethBolger", gtoken)
myDataContent = content(myData)
myDataDF = jsonlite::fromJSON(jsonlite::toJSON(myDataContent))
myDataDF$followers
myDataDF$public_repos

# The below code give specific details on the people following me

myFollowers = GET("https://api.github.com/users/ElizabethBolger/followers", gtoken)
myFollowersContent = content(myFollowers)
myFollowersDF = jsonlite::fromJSON(jsonlite::toJSON(myFollowersContent))
myFollowersDF$login
length = length(myFollowersDF$login)
length #Number of people following me

# The below code will give more information about my repositories

myRepos = GET("https://api.github.com/users/ElizabethBolger/repos", gtoken)
myReposContent = content(myRepos)
myReposDF = jsonlite::fromJSON(jsonlite::toJSON(myReposContent))
myReposDF$name
myReposDF$created_at

# The below allows you to view the data as JSON, the way it is done in browser

myDataJSon = toJSON(myDataDF, pretty = TRUE)
myDataJSon

# There are two methods of interrogating data. The above allows you to go through the JSON data.
# Below I am going to interrogate another user and put there data into a data.frame

# Using user 'khotyn' 

userData = GET("https://api.github.com/users/khotyn/followers?per_page=100;", gtoken)
stop_for_status(userData)

# Extract content from khotynss

extract = content(userData)

# Convert content to dataframe

githubDB = jsonlite::fromJSON(jsonlite::toJSON(extract))

# Subset dataframe

githubDB$login

# Retrieve a list of usernames

id = githubDB$login
user_ids = c(id)

# Create an empty vector and data.frame

users = c()
usersDB = data.frame(
  
  username = integer(),
  following = integer(),
  followers = integer(),
  repos = integer(),
  dateCreated = integer()
  
)

# Loop through users and find users to add to list

for(i in 1:length(user_ids))
{
  #Retrieve a list of individual users 
  followingURL = paste("https://api.github.com/users/", user_ids[i], "/following", sep = "")
  followingRequest = GET(followingURL, gtoken)
  followingContent = content(followingRequest)
  
  #Ignore if they have no followers
  if(length(followingContent) == 0)
  {
    next
  }
  
  followingDF = jsonlite::fromJSON(jsonlite::toJSON(followingContent))
  followingLogin = followingDF$login
  
  #Loop through 'following' users
  for(j in 1:length(followingLogin))
  {
    #Check that the user is not already in the list of users
    if(is.element(followingLogin[j], users) == FALSE)
    {
      #Add user to list of users
      users[length(users) + 1] = followingLogin[j]
      
      #Retrieve data on each user
      followingUrl2 = paste("https://api.github.com/users/", followingLogin[j], sep = "")
      following2 = GET(followingUrl2, gtoken)
      followingContent2 = content(following2)
      followingDF2 = jsonlite::fromJSON(jsonlite::toJSON(followingContent2))
      
      #Retrieve each users following
      followingNumber = followingDF2$following
      
      #Retrieve each users followers
      followersNumber = followingDF2$followers
      
      #Retrieve each users number of repositories
      reposNumber = followingDF2$public_repos
      
      #Retrieve year which each user joined Github
      yearCreated = substr(followingDF2$created_at, start = 1, stop = 4)
      
      #Add users data to a new row in dataframe
      usersDB[nrow(usersDB) + 1, ] = c(followingLogin[j], followingNumber, followersNumber, reposNumber, yearCreated)
      
    }
    next
  }
  #Stop when there are more than 200 users
  if(length(users) > 200)
  {
    break
  }
  next
}
#install.packages("plotly")
library(plotly)

#Link R to plotly. This creates online interactive graphs based on the d3js library
Sys.setenv("plotly_username"="ElizabethBolger")
Sys.setenv("plotly_api_key"="Lg3FKICVHpfQVL6JoOm8")

plot1 = plot_ly(data = usersDB, x = ~repos, y = ~followers, 
                text = ~paste("Followers: ", followers, "<br>Repositories: ", 
                              repos, "<br>Date Created:", dateCreated), color = ~dateCreated)
plot1

#Upload the plot to Plotly
Sys.setenv("plotly_username"="ElizabethBolger")
Sys.setenv("plotly_api_key"="Lg3FKICVHpfQVL6JoOm8")
api_create(plot1, filename = "Followers vs Repositories by Date")
#PLOTLY LINK: https://plot.ly/~ElizabethBolger/1

plot2 = plot_ly(data = usersDB, x = ~following, y = ~followers, 
                text = ~paste("Followers: ", followers, "<br>Following: ", 
                              following))
plot2

#Upload the plot to Plotly
Sys.setenv("plotly_username"="ElizabethBolger")
Sys.setenv("plotly_api_key"="Lg3FKICVHpfQVL6JoOm8")
api_create(plot2, filename = "Followers vs Following")
#PLOTLY LINK: https://plot.ly/~ElizabethBolger/4/


#LANGUAGES
#The following code finds the most popular language for each user

#Create empty vector
Languages = c()

id = myFollowersDF$login
user_ids = c(id)
user_ids

#Loop through all the users
for (i in 1:length(user_ids))
{
  #Access each users repositories and save in a dataframe
  RepositoriesUrl = paste("https://api.github.com/users/", user_ids[i], "/repos", sep = "")
  Repositories = GET(RepositoriesUrl, gtoken)
  RepositoriesContent = content(Repositories)
  RepositoriesDF = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent))
  
  #Find names of all the repositories for the given user
  RepositoriesNames = RepositoriesDF$name
  
  #Loop through all the repositories of an individual user
  for (j in 1: length(RepositoriesNames))
  {
    #Find all repositories and save in data frame
    RepositoriesUrl2 = paste("https://api.github.com/repos/", users[i], "/", RepositoriesNames[j], sep = "")
    Repositories2 = GET(RepositoriesUrl2, gtoken)
    RepositoriesContent2 = content(Repositories2)
    RepositoriesDF2 = jsonlite::fromJSON(jsonlite::toJSON(RepositoriesContent2))
    
    #Find the language which each repository was written in
    Language = RepositoriesDF2$language
    
    #Skip a repository if it has no language
    if (length(Language) != 0 && Language != "<NA>")
    {
      #Add the languages to a list
      Languages[length(Languages)+1] = Language
    }
    next
  }
  next
}

#Save the top 20 languages in a table
LanguageTable = sort(table(Languages), increasing=TRUE)

#Save this table as a data frame
LanguageDF = as.data.frame(LanguageTable)

#Plot the data frame of languages
plot3 = plot_ly(data = LanguageDF, x = LanguageDF$Languages, y = LanguageDF$Freq, type = "bar")
plot3

#Upload the plot to Plotly
Sys.setenv("plotly_username"="ElizabethBolger")
Sys.setenv("plotly_api_key"="2ezrFydJNHrMjAXSNtjX")
api_create(plot3, filename = "Most Popular Languages")
#PLOTLY LINK: https://plot.ly/~ElizabethBolger/6/#/


