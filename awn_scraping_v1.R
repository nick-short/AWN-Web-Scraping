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

library(tidyverse)
library(readr)
library(RSelenium)
#library(rvest) # We should consider using this in lieu of RSelenium for the queries
library(stringr) # We use str_split() and and str_replace() in get_number()
library(profvis) # We use pause()


          ## Source primary functions

source('primary_functions.R')

          ## Generate .RData files for storing data

## DO NOT RUN the code in this section unless starting a new set of searches
## from scratch as duplicate filenames will be overwritten 

## Define a list of search terms
terms <- list(c("driverless", "autonomous car", "self-driving car"),
              c("electric cars", "electric vehicles"),
              c("cloud computing"),
              c("solar panels","solar cells","solar electricity"),
              c("smartphone","smart phone", "iPhone"),
              c("3D printing","additive manufacturing"))

## Define a vector of corresponding filenames for storing data
filenames <- c("autonomous_cars.RData",
               "electric_cars.RData",
               "cloud_computing.RData",
               "solar_tech.RData",
               "smartphones.RData",
               "3D_printing.RData")

generate_datafiles(words = terms, files = filenames, nsnip = 50)


          ## Initiate AWN session with manual login

get_awn_session()
#twofa_login() # We currently do not use this; login manually

          ## Execute web scraping

## Load each data file, and scrape using stored URLs.  Set 'nsnip' to the number
## of snippets to be returned, or the function will only scrape hits.
execute_queries(file = "autonomous_cars.RData", nsnip = 50)
execute_queries(file = "electric_cars.RData", nsnip = 50)

execute_queries(file = "smartphones.RData", nsnip = 50)
execute_queries(file = "3D_printing.RData", nsnip = 50)


          ## Close the automated Chrome window

remDr$close()


## Get baseline hits (for normalization) if needed
urls <- generate_baseline_urls(beg_month = "Jan", beg_year = 1985, end_month = "Sep", end_year = 2018)
hits <- count_data(first_month, first_year, last_month, last_year)
save(urls, hits, file = "baseline.RData")
execute_queries(file = "baseline.RData")







## Search 4: Cloud Computing
words <- c("cloud computing")
words <- format_words(words)
word_string <- make_string(words)

urls <- generate_urls(first_month, first_year, last_month, last_year, word_string)
hits <- count_data(first_month, first_year, last_month, last_year)
snippets <- list()
save(urls, hits, snippets, file = "cloud_computing.RData")

start <- Sys.time()
execute_queries(file = "cloud_computing.RData", nsnip = 50)
fin <- Sys.time()
fin - start

## Search 5: Solar Tech
words <- c("solar panels","solar cells","solar electricity")
words <- format_words(words)
word_string <- make_string(words)

urls <- generate_urls(first_month, first_year, last_month, last_year, word_string)
hits <- count_data(first_month, first_year, last_month, last_year)
snippets <- list()
save(urls, hits, snippets, file = "solar_tech.RData")

start <- Sys.time()
execute_queries(file = "solar_tech.RData", nsnip = 50)
fin <- Sys.time()
fin - start






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
