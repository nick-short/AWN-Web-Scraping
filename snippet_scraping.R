### Snippet Scraping Code ###
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

## List of queries
new_terms <- list(c("drone","unmanned aerial vehicle","UAV"),
                  c("driverless","autonomous car","self-driving car", "autonomous cars",
                    "self-driving cars","selfdriving car","selfdriving cars",
                    "self-driving truck","self-driving trucks"),
                  c("electric car","electric vehicle","electric cars","electric vehicles",
                    "electric hybrid","lithium ion battery","lithium ion batteries",
                    "electric aircraft","liion batteries"),
                  c("cloud computing","cloud technology", "cloud resources","cloud storage",
                    "software as a service","cloud applications"),
                  c("solar panels","solar cells","solar electricity","solar inverter",
                    "solar power","solar cell","solar panel","photovoltaic"),
                  c("smartphone","smart phone","iPhone","mobile internet","smart phones",
                    "smartphones","samsung galaxy"),
                  c("3D printing","additive manufacturing","inkjet bioprinting"))

## List of filenames
new_filenames <- c("drones_snippets.RData", "autonomous_cars_snippets.RData", "electric_cars_snippets.RData", 
                   "cloud_computing_snippets.RData", "solar_tech_snippets.RData", "smartphones_snippets.RData", 
                   "3D_printing_snippets.RData")

## Generates datafiles - DON'T RUN THIS LINE, OR ELSE IT WILL CLEAR ALREADY-EXISTING FILES!
#generate_datafiles(test_words = new_terms, files = new_filenames, nsnip = 100, first_month = "Jan", 
#                   first_year = 2000)


### RUN THIS LINE IN TERMINAL ONCE DOCKER IS ACTIVATED TO ACTIVATE ENVIRONMENT!!!
#docker run -d -p 4445:4444 selenium/standalone-chrome

## Start up scraping internet environment
remDr <- get_awn_session()




##### Scrape here #####

# Already done #
execute_queries(file = "cloud_computing_snippets.RData", nsnip = 50)
execute_queries(file = "autonomous_cars_snippets.RData", nsnip = 50)
execute_queries(file = "electric_cars_snippets.RData", nsnip = 50)
execute_queries(file = "drones_snippets.RData", nsnip = 50)
execute_queries(file = "solar_tech_snippets.RData", nsnip = 50)
execute_queries(file = "smartphones_snippets.RData", nsnip = 50)
execute_queries(file = "3D_printing_snippets.RData", nsnip = 50)

#######################


## Load specific file to check snippets
load(file = "smartphones_snippets.RData")
length(snippets)
snippets

## Close internet environment
remDr$close()

### Save Snippet Files as CSVs ###
save_snippets_csv("cloud_computing_snippets.RData", 50, 
                  "~/Desktop/Research/Snippets_CSVs/cloud_computing_snippets.csv")
save_snippets_csv("autonomous_cars_snippets.RData", 50, 
                  "~/Desktop/Research/Snippets_CSVs/autonomous_cars_snippets.csv")
save_snippets_csv("electric_cars_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/electric_cars_snippets.csv")
save_snippets_csv("drones_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/drones_snippets.csv")
save_snippets_csv("solar_tech_snippets.RData", 50, 
                  "~/Desktop/Research/Snippets_CSVs/solar_tech_snippets.csv")
save_snippets_csv("smartphones_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/smartphones_snippets.csv")
save_snippets_csv("3D_printing_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/3D_printing_snippets.csv")
