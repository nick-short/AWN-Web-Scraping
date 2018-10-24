## This function will format the provided search 'words' by substituting "+" for
## white space

format_words <- function(terms){return(as.character(sapply(X = words, FUN = function(y){y <- gsub(" ", "+", y, fixed = TRUE)})))}

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
  url_suffix <- '&bln2=OR&fld2=alltext&val2=&bln3=OR&fld3=alltext&val3=&sort=YMD_date%3AD'
  
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

## This function well enter your username and password and advance the browser
## through the first stage of two-factor authentication.  Right now you have to
## manually pass the second stage.

twofa_login <- function(username, password){
  
  username_box <- remDr$findElement(using = 'css selector', "#username")
  username_box$sendKeysToElement(list(username))
  
  password_box <- remDr$findElement(using = 'css selector', "#password")
  password_box$sendKeysToElement(list(password))
  
  login_button <- remDr$findElement(using = 'css selector', ".submit-button")
  login_button$clickElement()
  
  # Need a line for the DUO push too (if that page pops up); for now enter manually
}

## This function will retrieve the number of search results

get_number <- function(){
  
  # Determine the number of search results and assign it to the variable "num"
  text_line <- remDr$findElement(using = 'css selector', ".nb-showing-result-count")
  text_num <- as.character(text_line$getElementText())
  
  # Text takes the form of "Showing 1-X of Y Results" where Y, the desired
  # number, is the fourth word. Extract Y, then remove commas and convert to
  # numeric format
  num <- unlist(str_split(text_num, " "))[4]
  num <- as.numeric(str_remove_all(num, ","))
  return(num)
}


# This function assumes we are on the first page of search query.  It will
# re-sort the data so that the best matches, rather than the most recent
# matches, are displayed first.  It will then change the page options so that
# 50, rather than 10, results are displayed on each page.  It will
# then retrieve 'num' text snippets.  If num is greater than rpp, the function
# will sequentially advance the browser page by page and extract the text
# snippets shown on each page.

get_snippets <- function(num, tot_results){
  cap <- min(num, tot_results, na.rm = TRUE)
  snippets <- rep(NA, cap)
  count <- 1
  rpp <- 50 # Define number of results per page
  
  # Re-sort so that the best matches are displayed first
  sort_dropdown <- remDr$findElement(using='css selector', "#result-sort")
  sort_dropdown$clickElement()
  
  relevant_first <- remDr$findElement(using = 'xpath', value = '//*[@id="resultsort-_rank_D"]')
  relevant_first$clickElement()
  
  # Change page options to display 50 result per page
  num_results_dropdown <- remDr$findElement(using='id', value="display-options")
  num_results_dropdown$clickElement()
  
  fifty_results <- remDr$findElement(using = 'xpath', '//*[@id="change-results-per-page-50"]')
  fifty_results$clickElement()
  
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
      
      next_button <- remDr$findElement(using = 'css selector', ".pager-next a")
      next_button$clickElement()
      
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

execute_queries <- function(url_vector, hits, text_list, nsnip){
  
  for(i in 1:length(url_vector)) {
    
    # Navigate to the URL
    remDr$navigate(url_vector[i])
    
    # We need a try-catch here in case there are no search results. 
    tryCatch({
      
      # Scrape number of hits and, if 'text_list' is passed to function, scrape snippets too
      tot_results <- get_number()
      hits$count[i] <- tot_results
      if (!is.null(text_list)){
        
        # If nsnip is not specified, set to total number of hits
        if (is.null(nsnip)){nsnip <- hits$count[i]} 
        text_list[[i]] <- get_snippets(nsnip, tot_results)
        }
      
    }, error = function(e) {
      
      # Otherwise set to 0 and NA respectively
      hits$count[i] <- 0
      if(!is.null(text_list)){text_list[[i]] <- "NA"}
      
    })
    
    # Pause before submitting another query.  At the moment, the pause is normally
    # distributed around 30 seconds, with a SD of 5 seconds.
    
    pause(abs(rnorm(1,30,5)))
    
  }
  
  if(is.null(text_list)){result <- hits} else{result <- list(hits,text_list)}
  return(result)
}
