#### Individual Terms Analysis Code ####
### Written by Matthew Colón ###


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


#### Individual Cloud Computing Term Hits Scraping ####

#cloud_terms <- list(c("cloud computing"), c("cloud technology"), c("cloud resources"), c("cloud storage"),
#                    c("software as a service"), c("cloud applications"), c("cloud services"), 
#                    c("cloud services"), c("cloud services"), c("cloud services"), c("cloud services"), c("cloud services"), 
#                    c("cloud services"), c("cloud services"), c("cloud services"))

cloud_filenames <- c("cloud_computing_term.RData", "cloud_technology_term.RData", "cloud_resources_term.RData", 
                     "cloud_storage_term.RData", "software_as_a_service_term.RData", "cloud_applications_term.RData")

#generate_datafiles(test_words = cloud_terms, files = cloud_filenames, nsnip = NULL, first_month = "Jan", 
#                   first_year = 2000)

### RUN THIS LINE IN TERMINAL ONCE DOCKER IS ACTIVATED!!!
#docker run -d -p 4445:4444 selenium/standalone-chrome

cloud_terms <- c("cloud computing","cloud technology", "cloud resources","cloud storage",
  "software as a service","cloud applications", "cloud services", "public cloud", "cloud business", "private cloud", 
  "cloud service", "hybrid cloud", "cloud platform", "cloud infrastructure", "cloud solutions", "cloud providers", 
  "cloud offering", "cloud revenue", "cloud offerings", "cloud solution", "cloud based", "service cloud", 
  "cloud customers", "cloud data", "enterprise cloud", "cloud environment")

remDr <- get_awn_session()

### This line of code will run the appropraite queries for each term, and create a data frame with the number
### of hits and proportion of hits for each term as well as the total number of hits for each month

#cloud_term_hits = create_individual_term_df(cloud_terms)
cloud_term_hits = term_hits

create_individual_term_plot(cloud_term_hits, cloud_terms, TRUE)

create_individual_term_plot(cloud_term_hits, cloud_terms, FALSE)


write_csv(cloud_term_hits, "~/Desktop/Research/updated_cloud_term_hits.csv")

remDr$close()
