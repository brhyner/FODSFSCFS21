source("Scripts/Init.R")

# Set up API essentials

NYT_KEY <- "GvmIE1Ahopng77DdKEfMnCZ3HkeOjyho"

baseurl <- paste0('http://api.nytimes.com/svc/search/v2/articlesearch.json?',
                             'begin_date=', begindate[1,1], # begin_date,
                             '&end_date=',   enddate[7,12], # end_date,
                             '&facet_filter=true&fq=news_desk%3A(%22Politics%22)', 
                             '&api-key=', NYT_KEY, 
                             #"&page=0", 
                             sep='')

initial_query <- fromJSON(baseurl)

# about 10000 hits which means 1000 pages

# since I can not iterate until page 1000, I might have to split the downloading process over the years
# for this I am going to define a matrix with the begin and end dates

# begin and end date vector specification ###################################
begindate <- as.data.frame(matrix(NA, nrow = 7, ncol = 12))

for (i in 1:7) {
  
  # define helper variable as I start in the year 2014
  helper <- i + 13
  
  # get year in 2014 format
  year <- paste0("20", helper)
  
  # name row
  # rownames(begindate[i]) <- year
  
  for (j in 1:12) {
    
    # get month in 01 format
    month <- str_pad(j, pad = 0, width = 2, "left")
    
    # name column 
    # colnames(begindate[j]) <- month
    
    # get days in 01 format
    days <- rep("01", 12)
    
    # paste everything into a string so it can be handled by the API
    begindate[i,j] <- paste0(year, month, days[j])
    
    
  }
  
}

# the same for the end date
enddate <- as.data.frame(matrix(NA, nrow = 7, ncol = 12))

for (i in 1:7) {
  
  # define helper variable as I start in the year 2014
  helper <- i + 13
  
  # get year in 2014 format
  year <- paste0("20", helper)
  
  # name row
  # rownames(enddate[i]) <- year
  
  for (j in 1:12) {
    
    # get month in 01 format
    month <- str_pad(j, pad = 0, width = 2, "left")
    
    # name column 
    # colnames(enddate[j]) <- month
    
    # get days of leap year
    if(year == "2016" | year == "2020") {
      
      days <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      
    }else{
      
      days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
      
    }
    
    # paste everything into a string so it can be handled by the API
    enddate[i,j] <- paste0(year, month, days[j])
    
  }
  
}


# get API stream running ###################

results <- list()

for (i in 1:7) {
  
  # first loop loops through the rows of the date data frames (= year)
  
  for (k in 1:12) {
    
    # second loop loops through the columns of the date data frames (= months)
    
    # initial query
    
    running_url <- paste0('https://api.nytimes.com/svc/search/v2/articlesearch.json?',
                             'begin_date=', begindate[i,k], 
                             '&end_date=', enddate[i,k],
                             '&facet_filter=true&fq=news_desk%3A(%22Politics%22)',
                             '&api-key=',NYT_KEY,
                             sep = '')
    
    
    # get max pages from initial query
    
    initial_Query <- fromJSON(running_url)
    
    maxPages <- round((initial_Query$response$meta$hits[1] / 10) - 1)
    
    
    # make sure maxPages is not negative
    
    if(maxPages < 0) {
      
      maxPages <- 0
      
    }
    
    # if there is only one page of results, I might run into problems with the rate limit, therefore
    
    Sys.sleep(6)
    
    # get results from page 0 to max by using a third for loop
    
    for (j in 0:maxPages) {
      
      # get results from page j
      
      nytSearch <- fromJSON(paste0(running_url, "&page=", j), flatten = T) %>%
        
        # save data in a data frame
        
        data.frame()
      
      
      # progress 
      
      print(paste0("Retrieving page ", j, " from ", month(ymd(begindate[i,k]), label = T), year(ymd(begindate[i,k]))))
      
      
      # save page searches
      
      results[[paste("Page", j, "in", month(ymd(begindate[i,k]), label = T), year(ymd(begindate[i,k])))]] <- nytSearch
      
      
      # pause for 6 seconds so the rate limit per minute is not violated
      
      Sys.sleep(7)
      
      
    }
    
    
    
    # progress
    
    print(paste0("Month ", k, " complete!"))
    
    
  }
  
  # progress
  
  print(paste0("Year ", i, " complete!"))
  
}

# December 2016 throws an error as there are no hits recorded which strikes me as odd, but this is what the API says
# similar errors a few times in 2017
# it might have been easier if I had implemented this for years instead of months

# Data gathering process complete 
# save image ####################

save.image("~/Foundations of Data Science for Social Scientists/Final Project/Foundations of Data Science for Social Scientist/Data_complete.RData")

# save all Data into one data.frame #################

politics_nyt <- bind_rows(results)

write_rds(politics_nyt, file = "trump_politics_nyt.Rda")
