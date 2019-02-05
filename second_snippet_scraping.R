### Second Snippet Scraping Code ###
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
final_terms <- list(c("driverless","autonomous car","self-driving car", "autonomous cars",
                    "self-driving cars","selfdriving car","selfdriving cars",
                    "self-driving truck","self-driving trucks", "autopilot"),
                    c("cloud computing","cloud technology", "cloud resources","cloud storage",
                    "software as a service","cloud applications", "cloud services", "public cloud", "cloud business", "private cloud", 
                    "cloud service", "hybrid cloud", "cloud platform", "cloud infrastructure", "cloud solutions", "cloud providers", 
                    "cloud offering", "cloud revenue", "cloud offerings", "cloud solution", "cloud based", "service cloud", 
                    "cloud customers", "cloud data", "enterprise cloud", "cloud environment"),
                    c("solar panels","solar cells","solar electricity","solar inverter",
                    "solar power","solar cell","solar panel","photovoltaic", "solar"),
                    c("next-generation genomics", "genomics", "genetic material", "genome", "Human Genome", "Human Genome Project", 
                      "synthetic biology", "genetic science", "biofuels", "gene sequencing", "genetically modified", "genesequencing machines", 
                      "genetically engineered", "DNA synthesis", "recombinant", "genetic engineering"), 
                    c("robot", "robotic", "automation", "reprogrammable machine", "automatic machine", "automatic arm", "automatic arms", 
                      "programmed motions", "automatic tasks", "motion controller", "motion controllers", "transfer arm", "transfer arms", 
                      "feedback sensor", "feedback sensors", "arm motion", "servo actuated", "gripping jaw", "harvesting arm", "harvesting arms", 
                      "positional servo system", "linear joint", "rotary joint", "track guided", "guided host", "cartesian arm", "cylindrical arm", 
                      "spherical arm", "system for arm", "end effector", "sensor actuated", "tactile sensor", "force feedback", "proximity sensor", 
                      "automatic welding", "automatic painting", "automatic handling"))

## NOTE: New Terms
# Drone - NONE (Use old snippets)
# 
# Driverless - "autopilot"
# 
# Electric - NONE (Use old snippets)
#
# Cloud - "cloud services", "public cloud", "cloud business", "private cloud", 
#"cloud service", "hybrid cloud", "cloud platform", "cloud infrastructure", "cloud solutions", "cloud providers", 
#"cloud offering", "cloud revenue", "cloud offerings", "cloud solution", "cloud based", "service cloud", 
#"cloud customers", "cloud data", "enterprise cloud", "cloud environment"
#
# Solar - "solar"
#
# Smartphone - NONE (Use old snippets)
#
# 3D printing - NONE (Use old snippets)
#
# Genomics - ALL
#
# Robotic - ALL

## List of filenames
final_filenames <- c("final_autonomous_cars_snippets.RData", "final_cloud_computing_snippets.RData", "final_solar_tech_snippets.RData", 
                     "final_genomics_snippets.RData", "final_robotic_snippets.RData")

## Generates datafiles - DON'T RUN THIS LINE, OR ELSE IT WILL CLEAR ALREADY-EXISTING FILES!
#generate_datafiles(test_words = final_terms, files = final_filenames, nsnip = 100, first_month = "Jan", 
#                   first_year = 2000)


### RUN THIS LINE IN TERMINAL ONCE DOCKER IS ACTIVATED TO ACTIVATE ENVIRONMENT!!!
#docker run -d -p 4445:4444 selenium/standalone-chrome

## Start up scraping internet environment
remDr <- get_awn_session()


##### Scrape here #####

# Already done #
execute_queries(file = "final_cloud_computing_snippets.RData", nsnip = 50)

## IN PROGRESS ##
execute_queries(file = "final_autonomous_cars_snippets.RData", nsnip = 50)

# Still to do #
execute_queries(file = "final_genomics_snippets.RData", nsnip = 50)
execute_queries(file = "final_robotic_snippets.RData", nsnip = 50)
execute_queries(file = "final_solar_tech_snippets.RData", nsnip = 50)

#######################


## Load specific file to check snippets
load(file = "final_cloud_computing_snippets.RData")
length(snippets)
snippets

## Close internet environment
remDr$close()

### Save Snippet Files as CSVs ###
save_snippets_csv("final_cloud_computing_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/final_cloud_computing_snippets.csv")
save_snippets_csv("final_autonomous_cars_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/final_autonomous_cars_snippets.csv")
save_snippets_csv("final_genomics_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/final_genomics_snippets.csv")
save_snippets_csv("final_robotic_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/final_robotic_snippets.csv")
save_snippets_csv("final_solar_tech_snippets.RData", 50, 
                  "~/Desktop/Research/Final_Snippets_CSVs/final_solar_tech_snippets.csv")
