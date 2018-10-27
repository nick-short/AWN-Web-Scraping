#### Access World News Scraping Code ####
### Written by Matthew Colón and Nicholas Short ###


rm(list = ls()) # Clear the workspace

          ## Libraries.  Note: we should probably use packrat for version control


#Use these commands to install RSelenium if needed.  Note: you have to install a
#Java development kit to make it run.  See:
#https://www.oracle.com/technetwork/java/javase/downloads/jdk10-downloads-4416644.html

#library(devtools) install_version("binman",
#version = "0.1.0", repos = "https://cran.uni-muenster.de/")
#install_version("wdman", version = "0.2.2", repos =
#"https://cran.uni-muenster.de/") install_version("RSelenium", version =
#"1.7.1", repos = "https://cran.uni-muenster.de/")

library(readr)
library(RSelenium)
#library(rvest) # Do we actually use this?
#library(stringi) # Do we actually use this?
library(stringr) # We use str_split() and and str_replace() in get_number()
library(profvis) # We use pause()
library(tidyverse)

          ## Source Primary Functions


source('primary_functions.R')


          ## Execute Web Scraping


## Create a vector of key search and ensure that entries with multiple words are
## in the proper format for constructing URLs
words <- c("driverless", "autonomous car", "self-driving car")
words <- format_words(words)
word_string <- make_string(words)

## Define first month and year and last month and year of search queries
first_month <- "Jan"
first_year <- 1980
last_month <- "Sep"
last_year <- 2018

## Construct search term URLs (urls) for all dates and baseline urls (base_urls)
## for determining total number of stories (with no search terms)
urls <- generate_urls(first_month, first_year, last_month, last_year, word_string)
base_urls <- generate_baseline_urls(first_month, first_year, last_month, last_year)

## Initialize variables for storing data
hits_data <- count_data(first_month, first_year, last_month, last_year)
sample_text <- list()

## Enter Stanford login information
usr <- ""
pwd <- ""

## Initiate automated Google Chrome window ("remDr")
driver<- rsDriver()
remDr <- driver[["client"]]

## Try to navigates to the first URL
remDr$navigate(urls[1])

## If this re-directs to login.stanford.edu, enter two-factor authorization info
## (DUO push still not enabled)
current_url <- unlist(remDr$getCurrentUrl())
if (grepl("login.stanford.edu", current_url)){twofa_login(usr,pwd)}

## You have to pause here to resolve duo push if needed.

## Load each URL and scrape the number of hits plus text snippets (get_snippets
## = sample_text).  Note: we will eventually want a function that runs a for
## loop on execute_queries for each set of search terms and then combines the
## results, but for now let's just execute 9 blocks of code.
start <- Sys.time()
autonomous_cars <- execute_queries(urls, hits_data, text_list = sample_text, nsnip = 50)
fin <- Sys.time()
fin - start
save(autonomous_cars, file = 'autonomous_cars.RData')

## Do the same for the baseline URLs, for normalization.  The reason for doing
## this separately is that it only needs to be executed once, as the raw monthly
## numbers will be the same (they do not contain search terms and will not vary
## by search terms).
start <- Sys.time()
baseline_results <- execute_queries(base_urls, hits_data)
fin <- Sys.time()
fin - start


## Search 3: Electric Cars
words <- c("electric cars", "electric vehicles")
words <- format_words(words)
word_string <- make_string(words)

urls <- generate_urls(first_month, first_year, last_month, last_year, word_string)
hits_data <- count_data(first_month, first_year, last_month, last_year)
sample_text <- list()
start <- Sys.time()
electric_cars <- execute_queries(urls, hits_data, text_list = sample_text, nsnip = 50)
fin <- Sys.time()
fin - start
save(electric_cars, file = 'electric_cars.RData')

## Search 4: Cloud Computing
words <- c("cloud computing")
words <- format_words(words)
word_string <- make_string(words)

urls <- generate_urls(first_month, first_year, last_month, last_year, word_string)
hits_data <- count_data(first_month, first_year, last_month, last_year)
sample_text <- list()
start <- Sys.time()
cloud_computing <- execute_queries(urls, hits_data, text_list = sample_text, nsnip = 50)
fin <- Sys.time()
fin - start
save(cloud_computing, file = 'cloud_computing.RData')

## Close the automated Chrome window
remDr$close()


          ## Save results.


## Save results to the current working directory.  Note: we want to think about
#how to do this in an automated way.  It might be best to run all the queries
#and then save.image(), or we might want to run them one at a time and save to a .csv.
#write.csv(query_results1, file = "QueryResultsTable.csv")


          ## Analyze results.


library(zoo)
library(ggplot2)

## Shape the data.  Convert dates into year-months.  Calculate hits relative to
## total news stories (from baseline_results) or a moving average of total results.

hits$date <- as.yearmon(hits$date, "%d%b%Y")
hits$prop <- hits$count / baseline_results$count
baseline_results$count_ma <- NA
ma_window <- 13 # Length of rolling mean spread
baseline_results$count_ma[ma_window:(nrow(baseline_results) - ma_window + 1)] <- rollmean(baseline_results$count, ma_window)
hits$prop_ma <- hits$count / baseline_results$count_ma

# This overlays the plots of the monthly average (blue) and 13-month moving
# average (red) of the query in question. It is likely that these won't be the
# plots we end up overlaying, but we have been discussing the matter of which is
# better to use, so at the moment, these plots are overlayed for comparison.
p <- ggplot(hits) + xlab("Time") + ylab("Proportion of Results") +
  labs(title = 'Proportion of Results for "Cloud Computing" on Access World News Time Series') +
  geom_line(aes(x = date, y = prop_ma, colour = "Yearly_Moving_Average")) + 
  geom_line(aes(x = date, y = prop, colour = "Standard_Monthly_Proportion")) + 
  scale_color_manual(name = "Type of Proportion", 
                     values = c(Yearly_Moving_Average = "red", 
                                Standard_Monthly_Proportion = "dark blue"))
p
