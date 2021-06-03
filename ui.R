library(shiny)
library(devtools)
library(twitteR)
library(plyr)
library(dplyr)
library(stringr)
library(wordcloud)
library(tidytext)
library(reshape2)
library(ggplot2)
library(plotly)
library(lubridate)
library(scales)
library(translate)
library(ggmap)
library(maptools)
library(maps)
library(leaflet)
library(stringi)
library(igraph)
library(visNetwork)
library(data.table)
library(mosaic)
library(RColorBrewer)
library(tidyr)
library(ggthemes)
library(tm)
library(zoo)
library(slam)
library(topicmodels)
library(reactable)
library(rtweet)
library(forestmangr)
library(tidyverse)
library(cowplot)
library(dplyr)
library(forestmangr)
library(tidyverse)
library(shiny)
library(rtweet)
library(dplyr)
library(BH)
library(forestmangr)
library(tidyverse)
library(tidytext)
library(wordcloud)
library(syuzhet)
library(hunspell)
# loading emoji data from today-is-a-good-day's emojis data accessible at https://github.com/today-is-a-good-day/emojis

# replace with appropriate personal keys
reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"


consumerKey <- "XXXX"
consumerSecret <- "XXXX"
acessToken <- "XXXX"
accessTokenSecret <- "XXXX"


setup_twitter_oauth(consumerKey, consumerSecret, acessToken, accessTokenSecret)

navbarPage(
    "Twitter Analytics", inverse = TRUE,
    tabPanel(
        "Welcome",
        h4(strong("Twitter Analytics App outlines trends in a user's twitter timeline,
                                 user's favorited tweets and topic related tweets. 
                                 This platform builds a portfolio of a user/topic through live tweet analysis,
                                 visualized via data tables, word clouds, bar plots, time series and network plots.")),
        
        HTML("<br/>"),
        
        hr(),
        
        HTML("<br/>"),
        h4(p(
            strong("Tweets Table"), "and", strong("Word Cloud"), "features visualize a 
                            user or topic's tweet through data table and word cloud.", strong("Tweet Statistics"),
            "performs analysis of a user's preferred tweet time and also categorises tweets in organic, retweet and replies.", strong("Topic Modeling"), "classifies words in topics.", strong("Sentiment Analysis"), "performs text analysis of user and topic related tweets, 
                            displaying the total number of tweets based on its associated emotion .",
            strong("Network Analysis"), "visualizes a user's associations and finds the 
                            most influential people in a domain by retweet analysis."
        ))
    ),
    navbarMenu(
        "Tweets Table",
        
        tabPanel(
            "User Tweets",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    # set app background color
                    tags$head(
                        tags$style(HTML("body {background-color: #9ae6fe;color: #000000;}"))
                    ),
                    
                    # changes color/formatting of the tab panel title
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='User Tweets'] {background-color: #333333;   color:white}")),
                    
                    tags$style(type = "text/css", "div.info.legend.leaflet-control br {clear: both;}"),
                    
                    strong("Feature shows tweets and user specific information (name, description, friends count, favorites count, 
                                                      account creation date)."),
                    
                    hr(),
                    
                    div(
                        style = "display: inline-block;vertical-align:right; width: 150px;",
                        textInput("twitterUser1", "Enter User", "")
                    ),
                    
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum1", "Tweet Number", 0)
                    ),
                    
                    actionButton("showUserTweets", "Show User Tweets", style = "color: #000000;background-color: #00aced;margin: 4px;"),
                    div(style = "display:inline-block", actionButton("showUserInfo", "Show User Info", style = "color: #000000;background-color: #00aced;margin: 4px;")),
                    
                    tags$head(
                        tags$style(HTML("#showUserTweets{font-weight:bold;}"))
                    ),
                    
                    tags$style(
                        type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"
                    ),
                    
                    tags$head(
                        tags$style(HTML("#showUserInfo{font-weight:bold;}"))
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "User Tweets",
                            dataTableOutput("userTweets2"),
                            dataTableOutput("userTweets1")
                        )
                    )
                )
            )
        ),
        tabPanel(
            "User Favorites",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='User Favorites'] {background-color: #333333;   color:white}")),
                    
                    strong("Feature shows favorited tweets by the specified user."),
                    
                    hr(),
                    
                    div(
                        style = "display: inline-block;vertical-align:right; width: 140px;",
                        textInput("twitterUser2", "Enter User", "")
                    ),
                    
                    div(
                        style = "display: inline-block;vertical-align:right; width: 70px;",
                        numericInput("tweetNum2", "Tweet Number", 0)
                    ),
                    
                    actionButton("showUserFav", "Show Tweets", style = "color: #000000;background-color: #00aced;margin: 4px;"),
                    
                    tags$head(
                        tags$style(HTML("#showUserFav{font-weight:bold;}"))
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "User Favorites",
                            dataTableOutput("userFavorite")
                        )
                    )
                )
            )
        ),
        tabPanel(
            "HashTag Tweets",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Tweets'] {background-color: #333333;   color:white}")),
                    
                    strong("Feature shows tweets for the specified topic."),
                    
                    hr(),
                    
                    div(
                        style = "display: inline-block;vertical-align:right; width: 140px;",
                        textInput("twitterUser3", "Enter word to search", "")
                    ),
                    
                    div(
                        style = "display: inline-block;vertical-align:right; width: 70px;",
                        numericInput("tweetNum3", "Number of Tweets", 0)
                    ),
                    
                    actionButton("showTopicMention", "Show Tweets", style = "color: #000000;background-color: #00aced;margin: 4px;"),
                    
                    
                    tags$head(
                        tags$style(HTML("#showTopicMention{font-weight:bold;}"))
                    )
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Hashtag Tweets", dataTableOutput("topicTweet"))
                    )
                )
            )
        )
    ),
    navbarMenu(
        "Tweet Statistics",
        tabPanel(
            "User Tweets",
            sidebarLayout(
                sidebarPanel(
                    strong("Feature assesses a user's twitter use across time metrics like hour, weekday, month and year ."),
                    
                    hr(),
                    textInput("twitterUser4", "Enter User", ""),
                    selectInput("chooseTime_Platform1", "Horizontal axis:", choices = c(
                        "Hour" = "hour", "Weekday" = "weekday", "Month" = "month",
                        "Year" = "year"
                    )),
                    selectInput("chooseTime_Platform2", "Vertical axis:", choices = c(
                        "Weekday" = "weekday", "Hour" = "hour", "Month" = "month",
                        "Year" = "year"
                    )),
                    numericInput("tweetNum4", "Tweet Number", 0),
                    
                    actionButton("showTweetStats", "Show Trends", style = "color: #000000;background-color: #00aced;"),
                    
                    tags$head(
                        tags$style(HTML("#showTweetStats{font-weight:bold;}"))
                    ),
                    
                    
                    downloadButton("downloadTweetStats", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Count Bar Plot",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Count Bar Plot'] {background-color: #333333;   color:white}")),
                            plotlyOutput("countBarPlot"),
                            dataTableOutput("countStatsClick")
                        ),
                        
                        tabPanel(
                            "Trends Over Time",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Line Plot'] {background-color: #333333;   color:white}")),
                            
                            dataTableOutput("overTimeTrends")
                        )
                    )
                )
            )
        ),
        tabPanel(
            "User Favorites",
            sidebarLayout(
                sidebarPanel(
                    strong("Feature assesses a user's tweet favorite behavior across time metrics like hour, weekday, month and year ."),
                    hr(),
                    textInput("twitterUser4_fav", "Enter User", ""),
                    selectInput("chooseTime_Platform1_fav", "Horizontal axis:", choices = c(
                        "Hour" = "hour", "Weekday" = "weekday", "Month" = "month",
                        "Year" = "year"
                    )),
                    selectInput("chooseTime_Platform2_fav", "Vertical axis:", choices = c(
                        "Weekday" = "weekday", "Hour" = "hour", "Month" = "month",
                        "Year" = "year"
                    )),
                    numericInput("tweetNum4_fav", "Tweet Number",0),
                    
                    actionButton("showTweetStats_fav", "Show Trends", style = "color: #000000;background-color: #00aced;"),
                    
                    tags$head(
                        tags$style(HTML("#showTweetStats_fav{font-weight:bold;}"))
                    ),
                    
                    
                    downloadButton("downloadTweetStats_fav", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Count Bar Plot",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Count Bar Plot'] {background-color: #333333;   color:white}")),
                            
                            plotlyOutput("countBarPlot_fav"),
                            dataTableOutput("countStatsClick_fav")
                        ),
                        
                        tabPanel(
                            "Trends Over Time",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Line Plot'] {background-color: #333333;   color:white}")),
                            
                            dataTableOutput("overTimeTrends_fav")
                        )
                    )
                )
            )
        ),
        
        tabPanel(
            "Category Plot",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='Category Plot'] {background-color: #333333;   color:white}")),
                    strong("Feature categorises tweets and shows plot of monthly and yearly usage."),
                    hr(),
                    
                    textInput("twitterUser0", "Enter User", ""),
                    tags$head(
                        tags$style(HTML("#CategoryPlot{font-weight:bold;}"))
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum0", "Tweet Number", 0)
                    ),
                    actionButton("CatPlot", "Show Chart", style = "color: #000000;background-color: #00aced;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "CategoryPlot", plotOutput("catPlot", height = "700px"),
                            plotlyOutput("catPlotTab", height = "800px")
                        )
                    ), width = 9
                )
            )
        )
    ),
    navbarMenu(
        "Word Cloud",
        tabPanel(
            "User Tweets",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='User Tweets'] {background-color: #333333;   color:white}")),
                    
                    strong("Shows word cloud of organic tweets"),
                    hr(),
                    
                    textInput("twitterUser5", "Enter User", ""),
                    tags$head(
                        tags$style(HTML("#showTweetCloud{font-weight:bold;}"))
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum5", "Tweet Number", 0)
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 70px;",
                        numericInput("minWords", "Min Words", 0)
                    ),
                    actionButton("showTweetCloud", "Show Cloud", style = "color: #000000;background-color: #00aced;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "User Tweets", plotOutput("userTweetsCloud", height = "700px"),
                            plotlyOutput("userTweetsCloudTab", height = "800px")
                        )
                    ), width = 9
                )
            )
        ),
        tabPanel(
            "User Favorites",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='User Favorites'] {background-color: #333333;   color:white}")),
                    
                    strong("Feature shows word cloud for a user's favorited tweets, excluding all retweets and replies to get more personalized words. Accompanying dotplot shows word counts. Specify tweet number and minimum 
                                                      number of repeated words (high number of minimum words will yield smaller and more relevant words in cloud)."),
                    hr(),
                    textInput("twitterUser6", "Enter User", ""),
                    
                    tags$head(
                        tags$style(HTML("#showFavoriteCloud{font-weight:bold;}"))
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum6", "Tweet Number", 0)
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 70px;",
                        numericInput("minWords2", "Min Words", 0)
                    ),
                    
                    actionButton("showFavoriteCloud", "Show Cloud", style = "color: #000000;background-color: #00aced;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "User Favorites", plotOutput("userFavoritesCloud", height = "500px"),
                            plotlyOutput("userFavoritesCloudTab", height = "900px")
                        )
                    )
                )
            )
        ),
        tabPanel(
            "Topic Tweets",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Tweets'] {background-color: #333333;   color:white}")),
                    
                    strong("Feature shows word cloud for the specified topic. Accompanying dotplot shows word counts. Specify tweet number and minimum 
                                                      number of repeated words (high number of minimum words will yield smaller and more relevant words in cloud)."),
                    hr(),
                    textInput("twitterUser7", "Enter Topic", ""),
                    
                    tags$head(
                        tags$style(HTML("#showTopicCloud{font-weight:bold;}"))
                    ),
                    div(style = "display: inline-block;vertical-align:top; width: 0.5px;", HTML("<br>")),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum7", "Tweet Number",0)
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 60px;",
                        numericInput("minWords3", "Min Words", 0)
                    ),
                    actionButton("showTopicCloud", "Show Cloud", style = "color: #000000;background-color: #00aced;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Topic Tweets", plotOutput("tweetTopicCloud", height = "700px"),
                            plotlyOutput("topicTweetsCloudTab", height = "700px")
                        )
                    )
                )
            )
        )
    ),
    navbarMenu(
        "Topic Modeling",
        tabPanel(
            "User Tweets",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='User Tweets'] {background-color: #333333;   color:white}")),
                    
                    strong("Feature shows most used words and their count in users tweet"),
                    
                    hr(),
                    textInput("twitterUser1_topic", "Enter User", ""),
                    tags$head(
                        tags$style(HTML("#showTopicModelUser{font-weight:bold;}"))
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum1_topic", "Tweet Number", 0)
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 70px;",
                        numericInput("topicNumber1_topic", "Number of Topics",0)
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 70px;",
                        numericInput("topWords1User", "Number of Words", 5)
                    ),
                    actionButton("showTopicModelUser", "Show Plot and Table", style = "color: #000000;background-color: #00aced;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Topic Count Plot ",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Count Plot'] {background-color: #333333;   color:white}")),
                            plotlyOutput("userTweetsWords", height = "500px")
                        ),
                        tabPanel(
                            "Topic Plot",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Plot'] {background-color: #333333;   color:white}")),
                            plotlyOutput("userTweetsTopic", height = "500px"),
                            div(
                                style = "display: inline-block;vertical-align:right; width: 200px;",
                                textInput("topicPlotTerm", "Enter term", "")
                            ),
                            div(
                                style = "display: inline-block;vertical-align:right; width: 200px;",
                                textInput("topicPlotTopic", "Enter topic","")
                            ),
                            dataTableOutput("userTweetsTopicRelated")
                        ),
                        tabPanel(
                            "Topic Table",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Table'] {background-color: #333333;   color:white}")),
                            dataTableOutput("userTweetsTopicTable")
                        ),
                        tabPanel(
                            "Zoom in Topic",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Zoom in Topic'] {background-color: #333333;   color:white}")),
                            numericInput("zoomUserTopic", "Topic Number", ""),
                            plotlyOutput("userTweetsTopicSpecific", height = "1700px", width = "800px")
                        )
                    )
                )
            )
        ),
        tabPanel(
            "Topic Tweets",
            sidebarLayout(
                sidebarPanel(
                    width = 3,
                    
                    tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Tweets'] {background-color: #333333;   color:white}")),
                    
                    strong("Feature performs topic modeling for the specified topic, displaying 
                                                prevalent words for the specified number of topics in plot and table formats.
            Specify number of topics and words per topic for display. 
            Decrease the number of topics and words per topic for a less crowded display. Search for a term and topic to see related tweets."),
                    
                    hr(),
                    textInput("twitterUser3_topic", "Enter Topic", ""),
                    
                    tags$head(
                        tags$style(HTML("#showTopicModelTweets{font-weight:bold;}"))
                    ),
                    div(style = "display: inline-block;vertical-align:top; width: 0.5px;", HTML("<br>")),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum3_topic", "Tweet Number",0)
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 60px;",
                        numericInput("topicNumber3_topic", "Number of Topics", 0)
                    ),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 70px;",
                        numericInput("topWords3User", "Number of Words", 0)
                    ),
                    actionButton("showTopicModelTweets", "Show Plot and Table", style = "color: #000000;background-color: #00aced;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Topic Plot",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Plot'] {background-color: #333333;   color:white}")),
                            plotlyOutput("topicTweetCloud", height = "500px"),
                            div(
                                style = "display: inline-block;vertical-align:right; width: 200px;",
                                textInput("topicTweetPlotTerm", "Enter term", "")
                            ),
                            div(
                                style = "display: inline-block;vertical-align:right; width: 200px;",
                                textInput("topicTweetPlotTopic", "Enter topic")
                            ),
                            dataTableOutput("topicsTweetsTopicRelated")
                        ),
                        tabPanel(
                            "Topic Table",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Topic Table'] {background-color: #333333;   color:white}")),
                            dataTableOutput("topicTweetCloudTable")
                        ),
                        tabPanel(
                            "Zoom in Topic",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Zoom in Topic'] {background-color: #333333;   color:white}")),
                            numericInput("zoomTopicSpecific", "Topic Number", ""),
                            plotlyOutput("topicTweetsSpecific", height = "1700px", width = "800px")
                        )
                    )
                )
            )
        )
    ),
    navbarMenu(
        "Sentiment Analysis",
        tabPanel(
            "Topic Tweets",
            sidebarLayout(
                sidebarPanel(
                    strong("Feature performs emotional analysis for the specified topic. 
            "),
                    hr(),
                    textInput("twitterUser9_topic", "Enter Topic", "0"),
                    div(
                        style = "display: inline-block;vertical-align:right; width: 80px;",
                        numericInput("tweetNum9_topic", "Tweet Number", 0)
                    ),
                    
                    tags$head(
                        tags$style(
                            HTML("#showWordBarSentiment3{margin-left:50px;}")
                        )
                    ),
                    
                    actionButton("showWordBarSentiment3", "Show Plots", style = "color: #000000;background-color: #00aced;margin: 4px;")
                ),
                mainPanel(
                    tabsetPanel(
                        tabPanel(
                            "Sentiment Analysis",
                            tags$style(HTML(".tabbable > .nav > li > a[data-value='Sentiment Analysis'] {background-color: #333333;   color:white}")),
                            plotOutput("sentimentWordCloud3")
                        )
                    )
                )
            )
        )
    ),
    navbarMenu(
        "Network Analysis",
        tabPanel(
            "Follower Network",
            sidebarLayout(
                sidebarPanel(
                    strong("Feature shows followers of the searched user. Magnitude of the 
                                           follower vertex corresponds to the number of followers for the followed user (larger
            vertex corresponds to more followers). Change the network display by specifying follower limit
            so users with large followers like celebrities and corporations 
            are balanced with individuals with normal twitter following. Additionally change the range to only
            show following personalities with followers within the range. Click on a vertex to display user-specific
            information about the follower as well as the tweet mentions involving the searched user 
            and the follower."),
                    hr(),
                    textInput("twitterUser15", "Search User", "hadleywickham"),
                    numericInput("tweetNum15", "Search Follower Number", 50),
                    numericInput("lowLimitFollower", "Follower Lower Limit", 10),
                    numericInput("upLimitFollower", "Follower Upper Limit", 1000),
                    numericInput("followerLimit2", "Follower Limit", 10),
                    actionButton("showFollowerNetwork", "Show Follower Network", style = "color: #000000;background-color: #00aced;"),
                    
                    tags$head(
                        tags$style(HTML("#showFollowerNetwork{font-weight: bold;}"))
                    ),
                    
                    downloadButton("downloadFollowerData", "Download Data", style = "color: #000000;background-color: #00aced;margin: 4px;")
                ),
                mainPanel(
                    visNetworkOutput("followerNetwork"),
                    dataTableOutput("followerNetworkUser"),
                    dataTableOutput("followerNetworkTweets")
                )
            )
        ),
        tabPanel(
            "Follower Location",
            sidebarLayout(
                sidebarPanel(
                    strong("Shows location of followers."),
                    hr(),
                    textInput("twitterUser161", "Search User", "0"),
                    numericInput("tweetNum161", "Search Follower Count", 0),
                    actionButton("showFollowerLocation", "Show Follower Location", style = "color: #000000;background-color: #00aced;"),
                    
                    tags$head(
                        tags$style(HTML("#showFollowerLocation{font-weight: bold;}"))
                    ),
                ),
                mainPanel(
                    plotlyOutput("FollowerLocation", height = "500px",width="1000px")
                    
                )
            )
        )
    )
)
