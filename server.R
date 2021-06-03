
function(input, output, session) {
    
    # Tweets Table Section
    
    ## User Tweets
    
    ### Returns tweets for specified user
    userTweetData <- eventReactive(input$showUserTweets, {
        withProgress(message = "Loading application", value = 0, {
            tweets <- userTimeline(input$twitterUser1, n = input$tweetNum1) # selects specified number of user tweets
            incProgress(0.7, detail = "Getting tweets")
            tab <- twListToDF(tweets) # converts tweets and associated metrics in table format
            tab2 <- tab[!duplicated(tab[, c("text")]), ] # removes duplicated text
            tab2 <- tab2 %>% dplyr::select(
                text, favoriteCount, replyToSN, created,retweetCount
            )
            incProgress(0.3, detail = "Finishing...")
            return(tab2)
        })
    })
    
    output$userTweets1 <- renderDataTable({
        userTweetData()
    })
    ### Returns info for specified twitter user
    userInfoData <- eventReactive(input$showUserInfo, {
        withProgress(message = "Loading application", value = 0, {
            search.string <- input$twitterUser1
            user <- getUser(search.string) # retrieve information about a Twitter user
            incProgress(0.4, detail = "Getting user data")
            ScreenName <- user$screenName
            Name <- user$name
            Description <- user$description
            FavoritesCount <- user$favoritesCount
            FriendsCount <- user$friendsCount
            StatusCount <- user$statusesCount
            AccountCreated <- date(user$created)
            AccountCreated <- as.character(AccountCreated)
            Location <- user$location
            incProgress(0.3, detail = "Processing")
            userInfo <- cbind(
                ScreenName, Name, Description,
                FriendsCount, StatusCount, FavoritesCount,
                AccountCreated, Location
            )
            incProgress(0.3, detail = "Finishing")
            return(userInfo)
        })
    })
    
    output$userTweets2 <- renderDataTable({
        userInfoData()
    })
    
    ## User Favorites
    userFavoriteData <- eventReactive(input$showUserFav, {
        withProgress(message = "Application loading", value = 0, {
            tweets <- favorites(input$twitterUser2, n = input$tweetNum2)
            incProgress(0.7, detail = "Getting your favorite tweets")
            tab <- twListToDF(tweets)
            tab2 <- tab[!duplicated(tab[, c("text")]), ]
            tab2 <- tab2 %>% dplyr::select(
                text, favoriteCount, replyToSN, created,screenName, retweetCount
            )
            incProgress(0.3, detail = "Finishing...")
            return(tab2)
        })
    })
    
    output$userFavorite <- renderDataTable({
        userFavoriteData()
    })
    
    ## Topic Tweets
    topicTweetsData <- eventReactive(input$showTopicMention, {
        withProgress(message = "Loading application", value = 0, {
            tweets <- searchTwitter(input$twitterUser3, n = input$tweetNum3)
            incProgress(0.7, detail = "Getting tweets")
            tab <- twListToDF(tweets)
            tab2 <- tab[!duplicated(tab[, c("text")]), ]
            tab2 <- tab2 %>% dplyr::select(
                text, favoriteCount, created, screenName, retweetCount
            )
            incProgress(0.3, detail = "Finishing...")
            return(tab2)
        })
    })
    
    output$topicTweet <- renderDataTable({
        topicTweetsData()
    })
    
    # End 
    
    # Tweet Statistics
    
    ## Tweet statistics for user
    
    tweetStatisticsData <- suppressWarnings(eventReactive(input$showTweetStats, {
        tweets <- userTimeline(input$twitterUser4, n = input$tweetNum4)
        tab <- twListToDF(tweets)
        tab$hour <- hour(with_tz(tab$created, "EST"))
        tab$date <- as.Date(tab$created)
        tab$year <- year(tab$date)
        tab$year <- as.factor(tab$year)
        tab$month <- as.factor(months(tab$date))
        tab$weekday <- as.factor(weekdays(tab$date))
        tab$weekday <- factor(tab$weekday, levels = c(
            "Sunday", "Monday",
            "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
        ))
        tab$hour <- as.factor(tab$hour)
        
        tab$statusSource <- as.factor(tab$statusSource)
        tab$statusSource <- regmatches(tab$statusSource, gregexpr("(?<=>).*?(?=<)", tab$statusSource, perl = TRUE))
        
        tab$weekday <- mapvalues(tab$weekday, from = c(
            "Sunday", "Monday", "Tuesday",
            "Wednesday", "Thursday", "Friday",
            "Saturday"
        ), to = c(
            "Sun", "Mon", "Tues", "Wed",
            "Thur", "Fri", "Sat"
        ))
        return(tab)
    }))
    
    ## Count Bar Plot to show twitter use trends over specified time/platform input
    output$countBarPlot <- renderPlotly({
        withProgress(message = "Application loading", value = 0, {
            incProgress(0.7, detail = "Getting tweets")
            tab <- tweetStatisticsData()
            join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1, input$chooseTime_Platform2) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
            join5$n <- as.numeric(join5$n)
            for (i in 1:nrow(join5))
            {
                join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
            }
            tab <- join5
            tab$count <- as.factor(as.character(tab$n))
            incProgress(0.3, detail = "Plotting")
            if ((input$chooseTime_Platform2) == "hour") {
                tab <- as.data.frame(tab)
                tab[, 2] <- as.numeric(as.character(tab[, 2]))
                p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1, y = "n", fill = input$chooseTime_Platform2)) +
                    geom_bar(stat = "identity") + theme_bw()
                ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot")
            }
            else {
                p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1, y = "n", fill = input$chooseTime_Platform2)) +
                    geom_bar(stat = "identity") + theme_bw()
                ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot")
            }
        })
    })
    
    ## Click on bar plot to show associated tweets at the specifed time/platform for user
    output$countStatsClick <- renderDataTable({
        d <- event_data("plotly_click", source = "countBarPlot")
        if (is.null(d)) {
            dat <- data.frame("Click on a bar to show associated tweets")
            colnames(dat) <- "Click on a bar to show associated tweets"
            dat[, 1] <- ""
            return(dat)
        }
        else {
            tab <- tweetStatisticsData()
            tab$weekday <- mapvalues(tab$weekday, from = c(
                "Su", "M", "Tu", "W",
                "Th", "F", "Sa"
            ), to = c(
                "Sunday", "Monday", "Tuesday",
                "Wednesday", "Thursday", "Friday",
                "Saturday"
            ))
            join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1, input$chooseTime_Platform2) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
            join5$n <- as.numeric(join5$n)
            for (i in 1:nrow(join5))
            {
                join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
            }
            tab <- join5
            tab$count <- as.factor(as.character(tab$n))
            tab1 <- tab[, c(1, 2)]
            tabChoice1 <- tab[, 1]
            tabChoice1 <- unique(tabChoice1)
            tabChoice1 <- as.data.frame(tabChoice1)
            tabChoice2 <- cbind(as.data.frame(tabChoice1), 1:length(tabChoice1[, 1]))
            colnames(tabChoice2) <- c(input$chooseTime_Platform1, "Variable1Val")
            tabChoice3 <- tabChoice1 %>% inner_join(tabChoice2)
            tabChoice3 <- tab %>% inner_join(tabChoice3)
            tabChoice3 <- as.data.frame(tabChoice3)
            if (input$chooseTime_Platform2 == "hour") {
                if (is.null(d)) {
                    dat <- as.data.frame("Click events appear here (double-click to clear)")[1]
                    return(dat)
                }
                else {
                    hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
                    hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
                    dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
                    dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
                    dat5 <- dat4[, c(1, 2, 4, 8)]
                    return(dat5)
                }
            }
            else {
                if (is.null(d)) {
                    dat <- as.data.frame("Click events appear here (double-click to clear)")[1][, 1]
                    return(dat)
                }
                else {
                    string <- paste("desc(", input$chooseTime_Platform2, ")", sep = "")
                    tabChoice3 <- tabChoice3 %>% arrange_(input$chooseTime_Platform1, string)
                    # return(tabChoice3)
                    hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
                    hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
                    if (is.null(hour5)) {
                        dat <- data.frame("Click on a bar to show associated tweets")
                        colnames(dat) <- "Click on a bar to show associated tweets"
                        dat[, 1] <- ""
                        return(dat)
                    }
                    else {
                        dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
                        dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
                        dat5 <- dat4[, c(1, 2, 4, 8)]
                        return(dat5)
                    }
                }
            }
        }
    })
    
    ## Table to show trends in twitter use over time
    output$overTimeTrends <- renderDataTable({
        withProgress(message = "Application loading", value = 0, {
            incProgress(0.7, detail = "Getting tweets")
            tab <- tweetStatisticsData()
            tab$weekday <- revalue(tab$weekday, c(
                "W" = "Wednesday", "Th" = "Thursday", "Sa" = "Saturday",
                "F" = "Friday", "Tu" = "Tuesday", "Su" = "Sunday", "M" = "Monday"
            ))
            tab1 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1, input$chooseTime_Platform2) %>% dplyr::summarise(n = n())
            incProgress(0.3, detail = "Plotting")
            return(data.frame(tab1))
        })
    })
    
    output$downloadTweetStats <- downloadHandler(
        filename = function() {
            paste("StatsData", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(tweetStatisticsData(), file)
        }
    )
    output$categoryChart <- renderPlotly({
        withProgress(message = "Application loading", value = 0, {
            incProgress(0.7, detail = "Getting tweets")
            tweets_data<-tweetStatisticsData%>%tweets
            organic_tweets<-tweets_data[tweets_data$is_retweet==FALSE]
            organic_tweets<-subset(organic_tweets,is.na(organic_tweets$reply_to_status_id))
            arrange(organic_tweets,desc(retweet_count))
            organic_tweets<-organic_tweets%>%arrange(-favoritesCount)
            retweet_tweets<-tweets_data[tweets_data$is_retweet==TRUE]
            replies_tweets<-subset(tweets_data,!is.na(tweets_data$reply_to_status_id))
            data<-data.frame( category = c("Organic","Retweet","Reply"),count=c(length(which(!is.na(organic_tweets$user_id))),which(!is.na(retweet_tweets$user_id)),which(!is.na(replies_tweets$user_id))))
            data$fraction = data$count /sum(data$count)
            data$percentage=data$count/sum(data$count)*100
            data$ymax=cumsum(data$fraction)
            data$ymin=c(0,head(data$ymax, n=-1))
            
            data=round_df(data,2)
            incProgress(0.3, detail = "Plotting")
            if ((input$chooseTime_Platform2) == "hour") {
                tab <- as.data.frame(tab)
                tab[, 2] <- as.numeric(as.character(tab[, 2]))
                p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1, y = "n", fill = input$chooseTime_Platform2)) +
                    geom_bar(stat = "identity") + theme_bw()
                ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot")
            }
            else {
                p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1, y = "n", fill = input$chooseTime_Platform2)) +
                    geom_bar(stat = "identity") + theme_bw()
                ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot")
            }
        })
    })
    ## Twitter statistics for user's favorited tweets
    tweetStatisticsData_fav <- suppressWarnings(eventReactive(input$showTweetStats_fav, {
        tweets <- favorites(input$twitterUser4_fav, n = input$tweetNum4_fav)
        tab <- twListToDF(tweets)
        tab$hour <- hour(with_tz(tab$created, "EST"))
        tab$date <- as.Date(tab$created)
        tab$year <- year(tab$date)
        tab$year <- as.factor(tab$year)
        tab$month <- as.factor(months(tab$date))
        tab$weekday <- as.factor(weekdays(tab$date))
        tab$weekday <- factor(tab$weekday, levels = c(
            "Sunday", "Monday",
            "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"
        ))
        tab$hour <- as.factor(tab$hour)
        
        tab$statusSource <- as.factor(tab$statusSource)
        tab$statusSource <- regmatches(tab$statusSource, gregexpr("(?<=>).*?(?=<)", tab$statusSource, perl = TRUE))
        tab$platform <- as.character(unlist(tab$statusSource))
        tab$platform <- as.factor(tab$platform)
        
        tab$platform <- gsub("Twitter Web Client", "Web Client", tab$platform)
        tab$platform <- gsub("Twitter for iPhone", "iPhone", tab$platform)
        tab$platform <- gsub("Twitter for Android", "Android", tab$platform)
        tab$platform <- gsub("Twitter for Mac", "Mac", tab$platform)
        tab$platform <- gsub("Twitter for iOS", "iOS", tab$platform)
        tab$platform <- gsub("Twitter for iPad", "iPad", tab$platform)
        tab$platform <- gsub("Tweetbot for Mac", "Mac bot", tab$platform)
        tab$platform <- gsub("Tweetbot for iOS", "iOS bot", tab$platform)
        tab$platform <- gsub("Meme_Twitterbot", "Meme bot", tab$platform)
        
        tab <- subset(tab, platform != "rtapp315156161")
        tab$weekday <- mapvalues(tab$weekday, from = c(
            "Sunday", "Monday", "Tuesday",
            "Wednesday", "Thursday", "Friday",
            "Saturday"
        ), to = c(
            "Su", "M", "Tu", "W",
            "Th", "F", "Sa"
        ))
        return(tab)
    }))
    
    ## Count Bar Plot to show twitter use trends over specified time/platform input
    output$countBarPlot_fav <- renderPlotly({
        withProgress(message = "Application loading", value = 0, {
            incProgress(0.7, detail = "Getting tweets")
            tab <- tweetStatisticsData_fav()
            join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_fav, input$chooseTime_Platform2_fav) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
            join5$n <- as.numeric(join5$n)
            for (i in 1:nrow(join5))
            {
                join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
            }
            tab <- join5
            tab$count <- as.factor(as.character(tab$n))
            incProgress(0.3, detail = "Plotting")
            if ((input$chooseTime_Platform2_fav) == "hour") {
                tab <- as.data.frame(tab)
                tab[, 2] <- as.numeric(as.character(tab[, 2]))
                p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1_fav, y = "n", fill = input$chooseTime_Platform2_fav)) +
                    geom_bar(stat = "identity") + theme_bw()
                ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot_fav")
            }
            else {
                p2 <- ggplot(tab, aes_string(x = input$chooseTime_Platform1_fav, y = "n", fill = input$chooseTime_Platform2_fav)) +
                    geom_bar(stat = "identity") + theme_bw()
                ggplotly(p2, tooltip = c("x", "fill", "y"), source = "countBarPlot_fav")
            }
        })
    })
    
    ## Click on bar plot to show associated tweets at the specifed time/platform for favorited tweets
    output$countStatsClick_fav <- renderDataTable({
        d <- event_data("plotly_click", source = "countBarPlot_fav")
        if (is.null(d)) {
            dat <- data.frame("Click on a bar to show associated tweets")
            colnames(dat) <- "Click on a bar to show associated tweets"
            dat[, 1] <- ""
            return(dat)
        }
        else {
            tab <- tweetStatisticsData_fav()
            tab$weekday <- mapvalues(tab$weekday, from = c(
                "Su", "M", "Tu", "W",
                "Th", "F", "Sa"
            ), to = c(
                "Sunday", "Monday", "Tuesday",
                "Wednesday", "Thursday", "Friday",
                "Saturday"
            ))
            join5 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_fav, input$chooseTime_Platform2_fav) %>% dplyr::summarise(word = paste(text, collapse = "a`"), n = n())
            join5$n <- as.numeric(join5$n)
            for (i in 1:nrow(join5))
            {
                join5$word[i] <- paste(unique((strsplit(join5$word[i], "\n"))[[1]]), collapse = "\n")
            }
            tab <- join5
            tab$count <- as.factor(as.character(tab$n))
            tab1 <- tab[, c(1, 2)]
            tabChoice1 <- tab[, 1]
            tabChoice1 <- unique(tabChoice1)
            tabChoice1 <- as.data.frame(tabChoice1)
            tabChoice2 <- cbind(as.data.frame(tabChoice1), 1:length(tabChoice1[, 1]))
            colnames(tabChoice2) <- c(input$chooseTime_Platform1_fav, "Variable1Val")
            tabChoice3 <- tabChoice1 %>% inner_join(tabChoice2)
            tabChoice3 <- tab %>% inner_join(tabChoice3)
            tabChoice3 <- as.data.frame(tabChoice3)
            if (input$chooseTime_Platform2_fav == "hour") {
                if (is.null(d)) {
                    dat <- as.data.frame("Click events appear here (double-click to clear)")[1]
                    return(dat)
                }
                else {
                    hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
                    hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
                    dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
                    dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
                    dat5 <- dat4[, c(1, 2, 4, 8)]
                    return(dat5)
                }
            }
            else {
                if (is.null(d)) {
                    dat <- as.data.frame("Click events appear here (double-click to clear)")[1][, 1]
                    return(dat)
                }
                else {
                    string <- paste("desc(", input$chooseTime_Platform2_fav, ")", sep = "")
                    tabChoice3 <- tabChoice3 %>% arrange_(input$chooseTime_Platform1_fav, string)
                    # return(tabChoice3)
                    hour4 <- tabChoice3 %>% group_by(Variable1Val) %>% mutate(cumsum = cumsum(n))
                    hour5 <- hour4 %>% subset(Variable1Val == d$x & cumsum == d$y)
                    if (is.null(hour5)) {
                        dat <- data.frame("Click on a bar to show associated tweets")
                        colnames(dat) <- "Click on a bar to show associated tweets"
                        dat[, 1] <- ""
                        return(dat)
                    }
                    else {
                        dat4 <- as.data.frame(hour5) %>% mutate(V3 = strsplit(as.character(word), "a`")) %>% unnest(V3)
                        dat4 <- plyr::rename(dat4, replace = c("V3" = "tweets"))
                        dat5 <- dat4[, c(1, 2, 4, 8)]
                        return(dat5)
                    }
                }
            }
        }
    })
    
    ## Table to show trends in twitter use over time
    output$overTimeTrends_fav <- renderDataTable({
        withProgress(message = "Application loading", value = 0, {
            incProgress(0.7, detail = "Getting tweets")
            tab <- tweetStatisticsData_fav()
            tab$weekday <- revalue(tab$weekday, c(
                "W" = "Wednesday", "Th" = "Thursday", "Sa" = "Saturday",
                "F" = "Friday", "Tu" = "Tuesday", "Su" = "Sunday", "M" = "Monday"
            ))
            tab1 <- tab %>% dplyr::group_by_(input$chooseTime_Platform1_fav, input$chooseTime_Platform2_fav) %>% dplyr::summarise(n = n())
            incProgress(0.3, detail = "Plotting")
            return(data.frame(tab1))
        })
    })
    
    output$downloadTweetStats_fav <- downloadHandler(
        filename = function() {
            paste("StatsData", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(tweetStatisticsData_fav(), file)
        }
    )
    userTweet <- eventReactive(input$CatPlot, {
        withProgress(message = "Loading application", value = 0, {
            tweets <- get_timeline(input$twitterUser0, n = input$tweetNum0) # selects specified number of user tweets
            incProgress(0.7, detail = "Getting tweets")
            #tab <- twListToDF(tweets) # converts tweets and associated metrics in table format
            incProgress(0.3, detail = "Finishing...")
            return(tweets)
            #count(organic),count(retweet),count(replies)
        })
    })
    output$catPlot<-renderPlot({
        withProgress(message = "Loading",value=0,{
            t<-userTweet()
            organic<-t[t$is_retweet==FALSE,]
            organic<-subset(organic,is.na(organic$reply_to_status_id))
            arrange(organic,desc(retweet_count))
            organic<-organic%>% arrange(-favorite_count)
            retweet<-t[t$is_retweet==TRUE,]
            replies1<-subset(t,!is.na(t$reply_to_status_id))
            data<-data.frame( category = c("Organic","Retweet","Reply"),count=c(length(which(!is.na(organic$user_id))),length(which(!is.na(retweet$user_id))),length(which(!is.na(replies1$user_id)))))
            data$fraction = data$count /sum(data$count)
            data$percentage=data$count/sum(data$count)*100
            data$ymax=cumsum(data$fraction)
            data$ymin=c(0,head(data$ymax, n=-1))
            data=round_df(data,2)
            Type_of_tweet<-paste(data$category,data$percentage,"%")
            p1<-ggplot(data,aes(ymax=ymax,ymin=ymin,xmax=4,xmin=3,fill=Type_of_tweet))+geom_rect()+coord_polar(theta="y")+xlim(c(2,4))+theme_void()+theme(legend.position="right")
            colnames(t)[colnames(t)=="screen_name"]<-"Twitter_Account"
            p2<-ts_plot(dplyr::group_by(t,Twitter_Account),"year")+ggplot2::theme_minimal()+ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))+ggplot2::labs(x=NULL,y=NULL,title="Frequency of tweets ",subtitle = "Tweet count aggregated by year ")
            p3<-ts_plot(dplyr::group_by(t,Twitter_Account),"month")+ggplot2::theme_minimal()+ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))+ggplot2::labs(x=NULL,y=NULL,title="Frequency of tweets ",subtitle = "Tweet count aggregated by month ")
            plot_grid(plotlist=list(p1,p2,p3),ncol=2)
        })
    })
    # Word Cloud
    
    ## Tweet word cloud for the specified user
    tweetsCloudData <- eventReactive(input$showTweetCloud, {
        tweets <- userTimeline(
            input$twitterUser5, n = input$tweetNum5, includeRts = FALSE,
            excludeReplies = TRUE
        )
        tab <- twListToDF(tweets)
        reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
        tab2 <- tab %>%
            dplyr::filter(!str_detect(text, "^RT")) %>%
            dplyr::mutate(text = str_replace_all(text, "http\\w+", "")) %>%
            unnest_tokens(word, text, token = "regex", pattern = reg) %>%
            dplyr::filter(
                !word %in% stop_words$word,
                str_detect(word, "[a-z]")
            )
        tab2 <- tab2 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
        return(tab2)
    })
    
    ## Plotting word cloud for topic tweets
    output$userTweetsCloud <- renderPlot({
        withProgress(message = "Application loading", value = 0, {
            tab <- tweetsCloudData()
            tab2 <- subset(tab, n >= input$minWords)
            wordcloud(
                words = tab2$word, freq = tab2$n, min.freq = input$minWords, scale = c(4.0,0.5), rot.per = 0.40,
                random.order = FALSE, colors=brewer.pal(8,"Dark2")
            )
        })
    })
    
    ## Table of raw word cloud counts for topic tweets
    output$userTweetsCloudTab <- renderPlotly({
        tab <- tweetsCloudData()
        incProgress(0.3, detail = "Building table")
        tab2 <- subset(tab, n >= input$minWords)
        tab2 <- plyr::rename(tab2, replace = c("word" = "Word"))
        tab2 <- plyr::rename(tab2, replace = c("n" = "WordCount"))
        incProgress(0.7, detail = "Finishing...")
        y.text <- element_text(size = 8)
        
        tab2$Word <- factor(tab2$Word, levels = tab2$Word[order(tab2$WordCount)])
        userTweets <- ggplot(tab2, aes(x = WordCount, y = Word)) + geom_point(size = 2, color = "#2575B7") +
            ggtitle("Word Cloud Count") + theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = y.text)
        ggplotly(userTweets, tooltip = c("x", "y"))
    })
    
    ## Favorites tweet word cloud for the specified user
    favoritesCloudData <- eventReactive(input$showFavoriteCloud, {
        tweets <- favorites(input$twitterUser6, n = input$tweetNum6)
        tab <- twListToDF(tweets)
        reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
        tab2 <- tab %>%
            dplyr::filter(!str_detect(text, "^RT")) %>%
            dplyr::mutate(text = str_replace_all(text, "http\\w+", "")) %>%
            unnest_tokens(word, text, token = "regex", pattern = reg) %>%
            dplyr::filter(
                !word %in% stop_words$word,
                str_detect(word, "[a-z]")
            )
        tab2 <- tab2 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
        return(tab2)
    })
    
    ## Plotting word cloud for favorite tweets
    output$userFavoritesCloud <- renderPlot({
        withProgress(message = "Application loading", value = 0, {
            col <- list(colorRampPalette(brewer.pal(9, "Blues"))(50))[[1]][37:50]
            incProgress(0.3, detail = "Building word cloud")
            tab <- favoritesCloudData()
            incProgress(0.7, detail = "Finishing...")
            tab2 <- subset(tab, n >= input$minWords2)
            wordcloud(
                words = tab2$word, freq = tab2$n, min.freq = input$minWords2, scale=c(4.0,.5), rot.per = 0.40,
                random.order = FALSE, colors=brewer.pal(8,"Dark2")
            )
        })
    })
    
    ## Table of raw word cloud counts for favorite tweets
    output$userFavoritesCloudTab <- renderPlotly({
        tab <- favoritesCloudData()
        incProgress(0.3, detail = "Building table")
        tab2 <- subset(tab, n >= input$minWords2)
        tab2 <- plyr::rename(tab2, replace = c("word" = "Word"))
        tab2 <- plyr::rename(tab2, replace = c("n" = "WordCount"))
        incProgress(0.7, detail = "Finishing...")
        y.text <- element_text(size = 8)
        
        tab2$Word <- factor(tab2$Word, levels = tab2$Word[order(tab2$WordCount)])
        favTweets <- ggplot(tab2, aes(x = WordCount, y = Word)) + geom_point(size = 2, color = "#2575B7") +
            ggtitle("Word Cloud Count") + theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = y.text)
        ggplotly(favTweets, tooltip = c("x", "y"))
    })
    
    ## Tweet word cloud for the specified topic
    topicCloudData <- eventReactive(input$showTopicCloud, {
        tweets <- searchTwitter(input$twitterUser7, n = input$tweetNum7)
        tab <- twListToDF(tweets)
        reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
        tab2 <- tab %>%
            dplyr::filter(!str_detect(text, "^RT")) %>%
            dplyr::mutate(text = str_replace_all(text, "http\\w+", "")) %>%
            unnest_tokens(word, text, token = "regex", pattern = reg) %>%
            dplyr::filter(
                !word %in% stop_words$word,
                str_detect(word, "[a-z]")
            )
        tab2 <- tab2 %>% dplyr::group_by(word) %>% dplyr::summarise(n = n()) %>% dplyr::arrange(desc(n))
        return(tab2)
    })
    
    output$tweetTopicCloud <- renderPlot({
        withProgress(message = "Application loading", value = 0, {
            col <- list(colorRampPalette(brewer.pal(9, "Blues"))(50))[[1]][37:50]
            incProgress(0.3, detail = "Building word cloud")
            tab <- topicCloudData()
            incProgress(0.7, detail = "Finishing...")
            tab2 <- subset(tab, n >= input$minWords3)
            wordcloud(
                words = tab2$word, freq = tab2$n, min.freq = input$minWords3, scale = c(4, 0.5), rot.per = 0.40,
                colors=brewer.pal(8,"Dark2"),random.order = FALSE
            )
        })
    })
    
    ## Table of raw word cloud counts for topic tweets
    output$topicTweetsCloudTab <- renderPlotly({
        tab <- topicCloudData()
        incProgress(0.3, detail = "Building table")
        tab2 <- subset(tab, n >= input$minWords3)
        tab2 <- plyr::rename(tab2, replace = c("word" = "Word"))
        tab2 <- plyr::rename(tab2, replace = c("n" = "WordCount"))
        incProgress(0.7, detail = "Finishing...")
        y.text <- element_text(size = 8)
        
        tab2$Word <- factor(tab2$Word, levels = tab2$Word[order(tab2$WordCount)])
        favTweets <- ggplot(tab2, aes(x = WordCount, y = Word)) + geom_point(size = 2, color = "#2575B7") +
            ggtitle("Word Cloud Count") + theme_bw() + theme(axis.title.y = element_blank(), axis.text.y = y.text)
        ggplotly(favTweets, tooltip = c("x", "y"))
    })
    
    # End of Word Cloud
    
    # Topic Modeling
    
    ## User Tweets
    
    userTweetsTopicData <- eventReactive(input$showTopicModelUser, {
        withProgress(message = "Application loading", value = 0, {
            tweets <- userTimeline(input$twitterUser1_topic, n = input$tweetNum1_topic)
            tab <- twListToDF(tweets)
            
            reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
            tab2 <- tab %>%
                dplyr::filter(!str_detect(text, "^RT")) %>%
                dplyr::mutate(text = str_replace_all(text, "http\\w+", ""))
            
            tweets <- sapply(tab2$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
            
            incProgress(0.6, detail = "Collecting Tweets...")
            corpus <- Corpus(VectorSource(tweets))
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, stripWhitespace)
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removeWords, stopwords("english"))
            tdm <- DocumentTermMatrix(corpus)
            
            term_tfidf <- tapply(tdm$v / row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm) / col_sums(tdm > 0))
            incProgress(0.4, detail = "Finishing...")
            tdm <- tdm[, term_tfidf >= 0.1]
            tdm <- tdm[row_sums(tdm) > 0, ]
            return(tdm)
        })
    })
    userTweetsWordData <- eventReactive(input$showTopicModelUser, {
        withProgress(message = "Application loading", value = 0, {
            tweets<-get_timeline(input$twitterUser1_topic,n=input$tweetNum1_topic)
            tweets$text<-gsub("https\\S*","",tweets$text)
            tweets$text<-gsub("@\\S*","",tweets$text)
            tweets$text<-gsub("amp","",tweets$text)
            tweets$text<-gsub("[\r\n]","",tweets$text)
            tweets$text<-gsub("[[:punct:]]","",tweets$text)
            return(tweets)
        })
    })
    output$userTweetsWords<-renderPlotly({
        tab<-userTweetsWordData()
        tweets<-tab%>%
            select(text)%>%
            unnest_tokens(word,text)
        tweets<-tweets%>%
            anti_join(stop_words)
        tweets%>%
            count(word,sort=TRUE)%>%
            top_n(15)%>%
            mutate(word=reorder(word,n))%>%
            ggplot(aes(x=word,y=n))+geom_col()+xlab(NULL)+coord_flip()+labs(y="Count",x="Unique Words", title = "Most frequent words found in tweets ")
    })
    output$userTweetsTopic <- renderPlotly({
        tdm <- userTweetsTopicData()
        
        chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        
        top_terms <- chapter_topics %>%
            group_by(topic) %>%
            top_n(input$topWords1User, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        top_terms$term <- as.factor(top_terms$term)
        topicPlot <- top_terms %>%
            mutate(term = reorder(term, beta)) %>%
            ggplot(aes(term, beta)) +
            facet_wrap(~ topic, scales = "free") + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 8))
        
        ggplotly(topicPlot)
    })
    
    output$userTweetsTopicSpecific <- renderPlotly({
        tdm <- userTweetsTopicData()
        
        chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        
        chapter_topic <- dplyr::filter(chapter_topics, topic == input$zoomUserTopic)
        top_terms <- chapter_topic %>%
            top_n(input$topWords1User, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        top_terms$term <- as.factor(top_terms$term)
        topicPlot <- top_terms %>%
            mutate(term = reorder(term, beta)) %>%
            ggplot(aes(term, beta)) + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 7), axis.title.y = element_blank())
    })
    
    output$userTweetsTopicRelated <- renderDataTable({
        tdm <- userTweetsTopicData()
        chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        tidiedTDM <- tidy(tdm)
        
        topicTweets <- chapter_topics %>% dplyr::inner_join(tidiedTDM)
        data <- dplyr::filter(topicTweets, term == input$topicPlotTerm & topic == input$topicPlotTopic)
        return(data)
    })
    
    output$userTweetsTopicTable <- renderDataTable({
        tdm <- userTweetsTopicData()
        
        chapters_lda <- LDA(tdm, k = input$topicNumber1_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        
        top_terms <- chapter_topics %>%
            group_by(topic) %>%
            top_n(input$topWords1User, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        return(top_terms)
    })
    
    ## Topic Tweets
    
    topicTweetsTopicData <- eventReactive(input$showTopicModelTweets, {
        withProgress(message = "Application loading", value = 0, {
            tweets <- searchTwitter(input$twitterUser3_topic, n = input$tweetNum3_topic)
            tab <- twListToDF(tweets)
            
            reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
            tab2 <- tab %>%
                dplyr::filter(!str_detect(text, "^RT")) %>%
                dplyr::mutate(text = str_replace_all(text, "http\\w+", ""))
            
            tweets <- sapply(tab2$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
            incProgress(0.6, detail = "Collecting Tweets...")
            
            corpus <- Corpus(VectorSource(tweets))
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, stripWhitespace)
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removeWords, stopwords("english"))
            tdm <- DocumentTermMatrix(corpus)
            
            term_tfidf <- tapply(tdm$v / row_sums(tdm)[tdm$i], tdm$j, mean) * log2(nDocs(tdm) / col_sums(tdm > 0))
            incProgress(0.4, detail = "Finishing...")
            
            tdm <- tdm[, term_tfidf >= 0.1]
            tdm <- tdm[row_sums(tdm) > 0, ]
            return(tdm)
        })
    })
    
    output$topicTweetCloud <- renderPlotly({
        tdm <- topicTweetsTopicData()
        
        chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        
        top_terms <- chapter_topics %>%
            group_by(topic) %>%
            top_n(input$topWords3User, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        top_terms$term <- as.factor(top_terms$term)
        topicPlot <- top_terms %>%
            mutate(term = reorder(term, beta)) %>%
            ggplot(aes(term, beta)) +
            facet_wrap(~ topic, scales = "free") + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 8))
        
        ggplotly(topicPlot)
    })
    
    output$topicTweetsSpecific <- renderPlotly({
        tdm <- topicTweetsTopicData()
        
        chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        
        chapter_topic <- dplyr::filter(chapter_topics, topic == input$zoomTopicSpecific)
        top_terms <- chapter_topic %>%
            top_n(input$topWords3User, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        top_terms$term <- as.factor(top_terms$term)
        topicPlot <- top_terms %>%
            mutate(term = reorder(term, beta)) %>%
            ggplot(aes(term, beta)) + coord_flip() + geom_point(size = 2, colour = "#00aced") + theme_bw() + theme(axis.text.y = element_text(size = 7), axis.title.y = element_blank())
    })
    
    output$topicsTweetsTopicRelated <- renderDataTable({
        tdm <- topicTweetsTopicData()
        chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        tidiedTDM <- tidy(tdm)
        
        topicTweets <- chapter_topics %>% dplyr::inner_join(tidiedTDM)
        data <- dplyr::filter(topicTweets, term == input$topicTweetPlotTerm & topic == input$topicTweetPlotTopic)
        return(data)
    })
    
    output$topicTweetCloudTable <- renderDataTable({
        tdm <- topicTweetsTopicData()
        
        chapters_lda <- LDA(tdm, k = input$topicNumber3_topic, control = list(seed = 200))
        chapter_topics <- tidy(chapters_lda, matrix = "beta")
        
        top_terms <- chapter_topics %>%
            group_by(topic) %>%
            top_n(input$topWords3User, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        
        return(top_terms)
    })
    
    # End of Topic Modeling
    
    # Sentiment Analysis
    ## Preparing data for topic tweet sentiment analysis
    topicSentimentData <- eventReactive(input$showWordBarSentiment3, {
        tab<-get_timeline(input$twitterUser9_topic,n=input$tweetNum9_topic)
        
        tab$text<-gsub("https\\S*","",tab$text)
        tab$text<-gsub("@\\S*","",tab$text)
        tab$text<-gsub("amp","",tab$text)
        tab$text<-gsub("[\r\n]","",tab$text)
        tab$text<-gsub("[[:punct:]]","",tab$text)
        tab1<-tab%>%
            select(text)%>%
            unnest_tokens(word,text)
        tab1<-tab1%>%
            anti_join(stop_words)
        tab1<-iconv(tab1,from="UTF-8",to="ASCII",sub="")
        tab1<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",tab1)
        tab1<-gsub("@\\w+","",tab1)
        return (tab1)
    })
    
    ## Building sentiment word cloud for topic tweets
    output$sentimentWordCloud3 <- renderPlot({
        withProgress(message = "Application loading", value = 0, {
            tweets<-topicSentimentData()
            ew_sentiment<-get_nrc_sentiment((tweets))
            sentimentscores<-data.frame(colSums(ew_sentiment[,]))
            names(sentimentscores)<-"Score"
            sentimentscores<-cbind("sentiment"=rownames(sentimentscores),sentimentscores)
            rownames(sentimentscores)<-NULL
            ggplot(data=sentimentscores,aes(x=sentiment,y=Score))+geom_bar(aes(fill=sentiment),stat="identity")+theme(legend.position = "none")+xlab("Sentiments")+ylab("Scores")+ggtitle("Total sentiment based scores")+theme_minimal()
        })
    })
    
    # End of Sentiment Analysis
    
    # Network Analysis
    
    ## Follower network
    
    ### Retweet data for network analysis
    followerNetworkData <- eventReactive(input$showFollowerNetwork, {
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Calculating follower data", value = 0)
        user <- getUser(input$twitterUser15)
        
        num <- input$tweetNum15
        
        follower <- user$getFollowers(num)
        full <- data.frame()
        
        for (i in 1:length(follower))
        {
            user2 <- getUser(follower[i])
            fcount <- user2$followersCount
            small <- cbind(user2$screenName, fcount)
            full <- rbind(small, full)
            progress$inc(1 / length(follower), detail = paste("Data for follower", i))
        }
        full2 <- list(rep(user$screenName, nrow(full)))
        names(full2)[1] <- "from"
        full3 <- cbind(full, full2)
        full3 <- plyr::rename(full3, replace = c("V1" = "to"))
        full3 <- plyr::rename(full3, replace = c("fcount" = "follower"))
        full3 <- full3 %>% dplyr::select(from, to, follower)
        return(full3)
    })
    
    ### Plots follower network
    output$followerNetwork <- renderVisNetwork({
        full3 <- followerNetworkData()
        
        full3 <- plyr::rename(full3, replace = c("follower" = "fcount"))
        full3$num <- as.numeric(as.character(full3$fcount))
        full4 <- full3 %>% dplyr::filter(num > input$lowLimitFollower & num < input$upLimitFollower)
        
        full3 <- full4
        
        full4 <- full3 %>% dplyr::select(from, to, fcount)
        full4$fcount <- as.numeric(as.character(full4$fcount))
        nodes <- full4$to
        nodes <- as.data.frame(nodes)
        names(nodes)[1] <- "id"
        nodes2 <- as.data.frame(setdiff(full3$from, nodes))
        names(nodes2)[1] <- "id"
        nodes <- rbind(nodes, nodes2)
        edges <- full4
        net <- graph_from_data_frame(d = edges, vertices = nodes, directed = T)
        V(net)$size <- E(net)$fcount / input$followerLimit2
        visNet <- toVisNetworkData(net, idToLabel = TRUE)
        visNetwork(nodes = visNet$nodes, edges = visNet$edges) %>%
            visNodes(color = list(highlight = "#FFFFFF")) %>%
            visEvents(select = "function(nodes) {
              Shiny.onInputChange('current_node_id2', nodes.nodes);
              ;}")
    })
    
    ### Retrives tweets with point click
    output$followerNetworkTweets <- renderDataTable({
        if (is.null(input$current_node_id2) == TRUE) {
            dat <- data.frame("Click vertex for follower info and related tweets")[1][, 1]
            colnames(dat) <- "Click vertex for follower info and related tweets"
            dat[, 1] <- ""
            return(dat)
        }
        else {
            full3 <- followerNetworkData()
            full4 <- full3 %>% dplyr::select(from, to, follower)
            full4$follower <- as.numeric(as.character(full4$follower))
            full4 <- plyr::rename(full4, replace = c("follower" = "NumberFriends"))
            f5 <- subset(full4, to == input$current_node_id2)
            person <- f5 %>% dplyr::select(to)
            tweets <- userTimeline(person, n = 10, excludeReplies = TRUE)
            tweetFr <- twListToDF(tweets)
            texts <- tweetFr$text
            create <- date(tweetFr$created)
            texts <- (as.data.frame(texts))
            create <- (as.data.frame(create))
            f6 <- cbind(f5, texts)
            f6 <- cbind(f6, create)
            as.data.table(f6)
        }
    })
    
    ### Retrieves user specific information for specified user
    output$followerNetworkUser <- renderDataTable({
        if (is.null(input$current_node_id2)) {
            dat <- data.frame("Click vertex for follower info and related tweets")[1][, 1]
            return(dat)
        }
        else {
            toNode <- input$current_node_id2
            user <- getUser(toNode)
            Categories <- c(
                "ScreenName", "Name", "Created", "Description", "Location", "FavoritesCount",
                "FollowersCount", "FriendsCount", "StatusesCount"
            )
            Values <- c(
                as.character(user$screenName), as.character(user$name),
                as.character(user$created),
                as.character(user$description), as.character(user$location),
                as.numeric(user$favoritesCount), as.character(user$followersCount),
                as.character(user$friendsCount), as.numeric(user$statusesCount)
            )
            return(as.data.frame(cbind(Categories, Values)))
        }
    })
    
    output$downloadFollowerData <- downloadHandler(
        filename = function() {
            paste("FollowerData", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(followerNetworkData(), file)
        }
    )
    followerLocationData <- eventReactive(input$showFollowerLocation, {
        
        num <- input$tweetNum161
        
        users<-search_users(input$twitterUser161,n=input$tweetNum161)
        return(users)
    })
    output$FollowerLocation<-renderPlotly({
        users<-followerLocationData()
        users %>%
            count(location, sort = TRUE) %>%
            mutate(location = reorder(location, n)) %>%
            top_n(20) %>%
            ggplot(aes(x = location, y = n)) +
            geom_col() +ggplot2::theme_minimal()+ggplot2::theme(plot.title=ggplot2::element_text(face="bold"))+
            coord_flip() +
            labs(x = "Location",
                 y = "Count",
                 title = "Where Twitter users are from - unique locations ")
    })
    ## End of Follower Network
    # End of Network Analysis
    
    output$Reference <- renderPrint({
        sessionInfo()
    })
}