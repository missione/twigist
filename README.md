# twigist
An application for Twitter user analysis in R . 
Project Details :
This is an application to analyse any Twitter Account with the analysis threshold of 800 tweets per use . It consist of analysis based on User Tweets(with favourite count , friends , likes , user tweet, and retweet count), User Usage Plot(monthly, weekly, hour wise, and yearly),Word Cloud of most used words in a user tweet, Word countbar plot , Topic and hashtag trends, PieChart of organic ,retweets and replies , Location of Followers , Sentiment analysis , and Network Analysis of followers . Here is the link : https://missione.shinyapps.io/Twigist/  for the working project .

I have used rtweet for extracting information from Twitter database via Twitter's REST in R  and shiny for making web app.There are various libraries and packages used in this project like syuzhet for sentiment analysis , ggplot for plotting ,visNetwork for network plotting etc. 

How to run it on local machine :
1.) Download R Studio .

2.) Go to R console and run : 

install.packages(c("shiny", "devtools", "twitteR", "plyr", "dplyr", "stringr", "wordcloud", "tidytext", "reshape2", "ggplot2", "plotly", "lubridate", "scales", "translate", "ggmap", "maptools", "maps", "leaflet", "stringi", "igraph", "visNetwork", "data.table", "mosaic", "RColorBrewer", "tidyr", "ggthemes", "tm", "zoo", "slam", "topicmodels", "reactable", "rtweet", "forestmangr", "tidyverse", "cowplot", "shiny", "BH", "syuzhet"))

This will install all the necessary packages .

3.) Fork and Clone the repository .

4.) Make a Twitter Developer Account and App and get the access and consumer keys. Here is the procedure for the same :https://medium.com/@divyeshardeshana/create-twitter-developer-account-app-4ac55e945bf4 .

5.) Update the keys and secret in ui.R .

6.) Click on run .

For details on how to use the website go to procedure.pdf .



