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
library(zoo)
library(ggplot2)


          ## Source primary functions

source('primary_functions.R')

          ## Generate .RData files for storing data

## DO NOT RUN the code in this section unless starting a new set of searches
## from scratch as duplicate filenames will be overwritten 

## Define a list of search terms
terms <- list(c("drone","unmanned aerial vehicle","UAV"),
              c("driverless","autonomous car","self-driving car", "autonomous cars",
                "self-driving cars","selfdriving car","selfdriving cars",
                "self-driving truck","self-driving trucks"),
              c("electric car","electric vehicle","electric cars","electric vehicles",
                "electric hybrid","lithium ion battery","lithium ion batteries",
                "electric aircraft","liion batteries"),
              c("cloud computing","cloud technology","cloud resources","cloud storage",
                "software as a service","cloud applications"),
              c("solar panels","solar cells","solar electricity","solar inverter",
                "solar power","solar cell","solar panel","photovoltaic"),
              c("smartphone","smart phone","iPhone","mobile internet","smart phones",
                "smartphones","samsung galaxy"),
              c("3D printing","additive manufacturing","inkjet bioprinting"))

## Define a vector of corresponding filenames for storing data
filenames <- c("drones.RData",
               "autonomous_cars.RData",
               "electric_cars.RData",
               "cloud_computing.RData",
               "solar_tech.RData",
               "smartphones.RData",
               "3D_printing.RData")

generate_datafiles(words = terms, files = filenames)

          ## Initiate AWN session with manual login


remDr <- get_awn_session()
#twofa_login() # We currently do not use this; login manually

          ## Execute substantive web scraping

## Load each data file, and scrape using stored URLs.  Set 'nsnip' to the number
## of snippets to be returned, or the function will only scrape hits.
execute_queries(file = "drones.RData")
execute_queries(file = "autonomous_cars.RData")
execute_queries(file = "electric_cars.RData") 
execute_queries(file = "cloud_computing.RData") 
execute_queries(file = "solar_tech.RData") 
execute_queries(file = "smartphones.RData")
execute_queries(file = "3D_printing.RData")

## Get baseline hits (for normalization) if needed
urls <- generate_baseline_urls(beg_month = "Jan", beg_year = 1985, end_month = "Sep", end_year = 2018)
hits <- count_data(beg_month = "Jan", beg_year = 1985, end_month = "Sep", end_year = 2018)
save(urls, hits, file = "baseline.RData")
execute_queries(file = "baseline.RData")

          ## Conduct tests for scraping time if desired

test_terms <- list(generate_test_terms(n = 20)) # n is the number of bi-grams
generate_datafiles(test_words = test_terms, files = c("testfile_0snip.RData"))
generate_datafiles(test_words = test_terms, files = c("testfile_50snip.RData"), nsnip = 50)
generate_datafiles(test_words = test_terms, files = c("testfile_100snip.RData"), nsnip = 100)

test_filenames <- c("testfile_50snip.RData",
                    "testfile_100snip.RData")
for (j in 1:length(test_filenames)) {
  load(file = test_filenames[j])
  timestamps <- vector(mode = "numeric", length = nrow(hits))
  if(exists("snippets")){save(hits, snippets, urls, timestamps, file = test_filenames[j])} else{
    save(hits, urls, timestamps, file = test_filenames[j])
  }
}

execute_queries(file = "testfile_0snip.RData")
execute_queries(file = "testfile_50snip.RData", nsnip = 50)
execute_queries(file = "testfile_100snip.RData", nsnip = 100)

load("testfile_0snip.RData")
hits_test <- c(0,timestamps[timestamps > 5])
rm(hits, urls, timestamps)
n <- length(hits_test)
hits_tbl <- vector(mode = "numeric",n)
for (i in 1:n){hits_tbl[i] <- sum(hits_test[1:i])}
hits_tbl <- tbl_df(hits_tbl) %>% 
  rename("elapsed_time" = "value") %>%
  mutate(elapsed_time = elapsed_time / 60) %>% # Convert seconds to minutes
  mutate(observations = c(1:n))

ggplot(hits_tbl, aes(x = observations, y = elapsed_time)) + geom_point()
summary(lm(elapsed_time ~ observations, data = hits_tbl))

          ## Close the automated Chrome window

remDr$close()

          ## Analyze results.

## Stitch the hit data together.  This will handle the  normalization (set the
## mvavg_win to a numeric for the length of the window in months if you want a
## moving average of baseline hits).  Data will be in wide format.
hits <- stitch_hits(filenames, basefile = "baseline.RData")

## Shape the data.  Convert dates into year-months.

hits$date <- as.yearmon(hits$date, "%d%b%Y")


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
