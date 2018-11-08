## This function will format the provided search 'words' by substituting "+" for
## white space

format_words <- function(terms){return(as.character(sapply(X = terms, FUN = function(y){y <- gsub(" ", "+", y, fixed = TRUE)})))}

## This function will take a vector of search words and create a single string
## to be used in constructing the URL

make_string <- function(word_vector){
  rep_str <- "%22"
  word_vector <- paste(rep_str, word_vector, rep_str, sep = "")
  word_string <- word_vector[1]
  for(i in 2:length(word_vector)) {word_string <- paste(word_string, word_vector[i], sep = "+%7C+")}
  return(word_string)
}

## This function will generate a full set of monthly URLS from 'beg_year' to
## 'end_year' given a formatted string of 'search_words'

generate_urls <- function(beg_month, beg_year, end_month, end_year, search_words){
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  years <- as.character(c(beg_year:end_year))
  
  months_rep <- rep(months, times = length(years))
  years_rep <- rep(years, each = length(months))
  all_dates <- paste(months_rep, "+", years_rep, sep = "")
  
  url_prefix <- 'https://infoweb-newsbank-com.stanford.idm.oclc.org/resources/search/nb?p=AWNB&b=results&action=search&fld0=YMD_date&val0='
  url_middle <- '&bln1=AND&fld1=alltext&val1='
  url_suffix <- '&bln2=OR&fld2=alltext&val2=&bln3=OR&fld3=alltext&val3=&sort=_rank_%3AD&maxresults=50&page=0'
  
  urls <- paste(url_prefix, all_dates, url_middle, search_words, url_suffix, sep = "")
  #urls <- gsub("\\\\", "", urls, fixed = TRUE)
  
  first_element <- which(all_dates == paste(beg_month, "+", beg_year, sep = ""))
  last_element <- which(all_dates == paste(end_month, "+", end_year, sep = ""))
  urls <- urls[first_element:last_element]
  
  return(urls)
}

## This function will generate a full set of monthly URLS from 'beg_year' to
## 'end_year' with no search words (for pulling total counts)

generate_baseline_urls <- function(beg_month, beg_year, end_month, end_year){
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  years <- as.character(c(beg_year:end_year))
  
  months_rep <- rep(months, times = length(years))
  years_rep <- rep(years, each = length(months))
  all_dates <- paste(months_rep, "+", years_rep, sep = "")
  
  url_prefix <- 'https://infoweb-newsbank-com.stanford.idm.oclc.org/resources/search/nb?p=AWNB&b=results&action=search&fld0=YMD_date&val0='
  url_suffix <- '&bln1=AND&fld1=YMD_date&val1=&sort=YMD_date%3AD'
  
  urls <- paste(url_prefix, all_dates, url_suffix, sep = "")
  #urls <- gsub("\\\\", "", urls, fixed = TRUE)
  
  first_element <- which(all_dates == paste(beg_month, "+", beg_year, sep = ""))
  last_element <- which(all_dates == paste(end_month, "+", end_year, sep = ""))
  urls <- urls[first_element:last_element]
  
  return(urls)
}


## This function will generate a data frame for storing the monthly counts with
## the 'date' variable in .Date format

count_data <- function(beg_month, beg_year, end_month, end_year){
  
  months <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  years <- as.character(c(beg_year:end_year))
  
  months_rep <- rep(months, times = length(years))
  years_rep <- rep(years, each = length(months))
  all_dates <- paste("1", months_rep, years_rep, sep = "")
  all_dates <- as.Date(all_dates, format = "%d%b%Y")
  
  first_element <- which(all_dates == as.Date(paste("1", beg_month, beg_year, sep = ""), format = "%d%b%Y"))
  last_element <- which(all_dates == as.Date(paste("1", end_month, end_year, sep = ""), format = "%d%b%Y"))
  all_dates <- all_dates[first_element:last_element]
  
  count_df <- data.frame(date = all_dates, count = rep(NA, length(all_dates)))
}

## This function will initiate a session with the AWN website using a test URL

get_awn_session <- function(test_url = NULL) {
  if(is.null(test_url)){test_url <- "https://infoweb-newsbank-com.stanford.idm.oclc.org/resources/search/nb?p=AWNB&b=results&action=search&fld0=YMD_date&val0=Jan+1980&bln1=AND&fld1=YMD_date&val1=&sort=YMD_date%3AD"}
  driver<- rsDriver()
  remDr <- driver[["client"]]
  remDr$navigate(test_url)
  return(remDr)
}

## This function check if the current URL is at login.stanford.edu, and if it
## is, it will enter your username and password and advance the browser through
## the first stage of two-factor authentication.  Right now you have to manually
## pass the second stage (with a duo push, for example).  We do not use this
## function at the moment because the code is on a public repository on github
## and we do not want usernames and passwords stored there.

twofa_login <- function(username, password){
  
  
  current_url <- unlist(remDr$getCurrentUrl())
  if (grepl("login.stanford.edu", current_url)){
  
    username_box <- remDr$findElement(using = 'css selector', "#username")
    username_box$sendKeysToElement(list(username))
    
    password_box <- remDr$findElement(using = 'css selector', "#password")
    password_box$sendKeysToElement(list(password))
    
    login_button <- remDr$findElement(using = 'css selector', ".submit-button")
    login_button$clickElement()
    
    # Need a line for the DUO push too (if that page pops up); for now enter manually
  }
}

## This function will take a list of search terms and a vector data file names,
## and it will generate .RData files for storing results.  Note: this function
## needs to be tested.

generate_datafiles <- function(test_words, files, nsnip = NULL, first_month = NULL, first_year = NULL, last_month = NULL, last_year = NULL){
  
  words_len <- length(test_words)
  files_len <- length(files)
  if(words_len != files_len) stop("Word list and file vector have different lengths.")
  
  ## Set default values if not passed to function
  if(is.null(first_month)){first_month = "Jan"}
  if(is.null(last_month)){last_month = "Sep"}
  if(is.null(first_year)){first_year = 1985}
  if(is.null(last_year)){last_year = 2018}
  
  for (j in 1:words_len){
    
    ## Format words and create single string of search terms
    word_vec <- test_words[[j]]
    word_vec <- format_words(word_vec)
    word_string <- make_string(word_vec)
    
    ## Initialize variables for storing data
    urls <- generate_urls(first_month, first_year, last_month, last_year, word_string)
    hits <- count_data(first_month, first_year, last_month, last_year)
    
    if(is.null(nsnip)){save(urls, hits, file = files[j])} else{
      snippets <- list()
      save(urls, hits, snippets, file = files[j])
    }
  }
}

## This function will simply return a vector of n (up to 240) bi-grams used for
## testing implementation time

generate_test_terms <- function(n){
  if(n > 240) stop("n must be less than 240")
  load(file = "test_terms.RData")
  return(terms$bigrams[1:n])
}


## This function will retrieve the number of search results

get_number <- function(){
  
  # Determine the number of search results and assign it to the variable "num"
  num <- tryCatch({
    suppressMessages({
      text_line <- remDr$findElement(using = 'css selector', ".nb-showing-result-count")
      text_num <- as.character(text_line$getElementText())
  
      # Text takes the form of "Showing 1-X of Y Results" where Y, the desired
      # number, is the fourth word. Extract Y, then remove commas and convert to
      # numeric format
      num <- unlist(str_split(text_num, " "))[4]
      num <- as.numeric(str_remove_all(num, ","))
      num
    })
    }, error = function(e) {0}
  )
  return(num)
}


# This function assumes we are on the first page of search query.  It will
# re-sort the data so that the best matches, rather than the most recent
# matches, are displayed first.  It will then change the page options so that
# 50, rather than 10, results are displayed on each page.  It will
# then retrieve 'num' text snippets.  If num is greater than rpp, the function
# will sequentially advance the browser page by page and extract the text
# snippets shown on each page.

get_snippets <- function(num, tot_results, curr_url){
  cap <- min(num, tot_results, na.rm = TRUE)
  snippets <- rep(NA, cap)
  count <- 1
  rpp <- 50 # Define number of results per page
  
  # Re-sort so that the best matches are displayed first
  #sort_dropdown <- remDr$findElement(using='css selector', "#result-sort")
  #sort_dropdown$clickElement()
  
  #relevant_first <- remDr$findElement(using = 'xpath', value = '//*[@id="resultsort-_rank_D"]')
  #relevant_first$clickElement()
  
  # Change page options to display 50 result per page
  #num_results_dropdown <- remDr$findElement(using='id', value="display-options")
  #num_results_dropdown$clickElement()
  
  #fifty_results <- remDr$findElement(using = 'xpath', '//*[@id="change-results-per-page-50"]')
  #fifty_results$clickElement()
  
  # If there are rpp or less results then simply take text snippets from the
  # num results; otherwise iterate through each page
  
  if (cap <= rpp) {
    
    for(j in 1:cap) {
      css <- paste("li:nth-child(", j, ") .preview", sep = "")
      text_box <- remDr$findElement(using = 'css selector', css)
      text_snippet <- as.character(text_box$getElementText())
      snippets[count] <- text_snippet
      count <- count + 1
    }
    
  } else {
    
    #start <- Sys.time()
    num_floor <- floor(num / rpp)
    for(k in 1:(num_floor)) {
      
      for(j in 1:rpp) {
        css <- paste("li:nth-child(", j, ") .preview", sep = "")
        text_box <- remDr$findElement(using = 'css selector', css)
        text_snippet <- as.character(text_box$getElementText())
        snippets[count] <- text_snippet
        count <- count + 1
      }
      
      if(k == 1) {
        sub("page=0", "", curr_url)
        sub("nb?", paste0("nb?page=", k ,"&"), curr_url)
      } else {
        sub(paste0("nb?page=", k-1), paste0("nb?page=", k), curr_url)
      }
      
      remDr$navigate(curr_url)
      
      #next_button <- remDr$findElement(using = 'css selector', ".pager-next a")
      #next_button$clickElement()
      
    }
    
    # The last page will contain (num %% rpp) results
    
    for(l in 1:(num %% rpp)) {
      css <- paste("li:nth-child(", l, ") .preview", sep = "")
      text_box <- remDr$findElement(using = 'css selector', css)
      text_snippet <- as.character(text_box$getElementText())
      snippets[count] <- text_snippet
      count <- count + 1
    }
    
    #fin <- Sys.time()
    #fin - start  
    
  }
  
  return(snippets)
  
}


## This function will query each url in 'url_vector', obtain the total number of
## news stories generated from that query using get_number(), and save the
## results to the corresponding row in the data frame 'hits'.  If an empty list
## is passed to the function, it will also extrapolate 'nsnip' text snippets
## using get_snippets().  If 'nsnip' is not specified, it will extrapolate all
## snippets.  It takes a pause of random length at the end of each query. The
## output will depend on how the function is called: (1) if only scraping hits,
## the 'hits' data frame will be returned; (2) if also scraping snippets, a list
## with the 'hits' data frame as the first element and the list of snippets as
## the second element will be returned.
execute_queries <- function(file, nsnip = NULL){
  
  # Load the file where results are to be written.  This file must contain a
  # 'hits' dataframe and a 'urls' vector.  It can also contain a 'snippets' list.
  load(file = file)
  
  # Set start_time if we are tracking time (if variable "time" exists in the file)
  if (exists("timestamps")){start_time <- Sys.time()}
  
  # Identify the first NA in hits$count.  If there are no results written (index
  # == 1), start at the first row / URL; otherwise, start one row prior to the
  # last recorded count result
  index <- min(which(is.na(hits$count))) 
  if(index == 1){start <- 1} else{start <- (index - 1)}
  
  for(i in start:length(urls)) {
    
    # Navigate to the URL
    remDr$navigate(urls[i])
    
    # Scrape number of hits 
    tot_results <- get_number()
    hits$count[i] <- tot_results
  
    # If 'snippets' is included in 'file', scrape snippets too      
    if (exists("snippets")){
      
      if (tot_results > 0){ # If total results is positive, scrape
        
        if (is.null(nsnip)){nsnip <- tot_results} # If nsnip is not specified, set to total number of hits
          snippets[[i]] <- get_snippets(nsnip, tot_results, urls[i])
          } else {snippets[[i]] <- NA} # Otherwise, set to NA
      }
    
    # Calculate time differential and reset clock on start_time
    if (exists("timestamps")){
      timestamps[i] <- difftime(Sys.time(), start_time, units = "secs")
      start_time <- Sys.time()
    }
    
    # Every tenth observation, and on the last observation, save results
    if ((i %% 5 == 0) | i == length(urls)){
      if (exists("timestamps")){
        if (exists("snippets")){save(urls, hits, snippets, timestamps, file = file)} else{save(urls, hits, timestamps, file = file)}} else {
        if (exists("snippets")){save(urls, hits, snippets, file = file)} else{save(urls, hits, file = file)}}
    }
    
    # Pause before submitting another query.  At the moment, the pause is normally
    # distributed around 30 seconds, with a SD of 5 seconds.
    
    pause(abs(rnorm(1,30,5)))
    
  }
  
}

## This function will stitch together the hit counts from a list of files that
## contain dataframe named 'hits', normalize them according to the hits stored
## in a baseline file if specified and return a dataframe with one column for
## each group of search terms.  The first file in the list is used to determine
## the number of rows in the final dataframe, and to pass the vector of dates.

stitch_hits <- function(filelist, basefile = NULL, mvavg_win = NULL){
  ncols <- length(filelist)
  load(filelist[[1]])
  nrows <- nrow(hits)
  varnames <- str_split(filelist, pattern = fixed("."), simplify = TRUE)[, 1] # Keep everything before period as a variable name
  results <- data.frame(matrix(data = NA, nrow = nrows, ncol = (ncols + 1)))
  colnames(results) <- c("date",varnames)
  results[,1] <- hits$date
  results[,2] <- hits$count
  
  for (j in 3:(ncols+1)){
    load(filelist[(j-1)])
    results[,j] = hits$count}
  
  if(!is.null(basefile)){
    load(basefile)
    if(is.null(mvavg_win)){for (k in 2:(ncols+1)){results[,k] <- results[,k] / hits$count}} else{
      ## Implement moving average here.  Code below copied/pasted and untested.
      #baseline_results$count_ma <- NA
      #ma_window <- 13 # Length of rolling mean spread
      #baseline_results$count_ma[ma_window:(nrow(baseline_results) - ma_window + 1)] <- rollmean(baseline_results$count, ma_window)
      #hits$prop_ma <- hits$count / baseline_results$count_ma
      
    }
  }
}