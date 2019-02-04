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

cloud_terms <- list(c("cloud computing"), c("cloud technology"), c("cloud resources"), c("cloud storage"),
                    c("software as a service"), c("cloud applications"))

cloud_filenames <- c("cloud_computing_term.RData", "cloud_technology_term.RData", "cloud_resources_term.RData", 
                     "cloud_stdorage_term.RData", "software_as_a_service_term.RData", "cloud_applications_term.RData")

#generate_datafiles(test_words = cloud_terms, files = cloud_filenames, nsnip = NULL, first_month = "Jan", 
#                   first_year = 2000)

### RUN THIS LINE IN TERMINAL ONCE DOCKER IS ACTIVATED!!!
#docker run -d -p 4445:4444 selenium/standalone-chrome

remDr <- get_awn_session()

execute_queries(file = "cloud_computing_term.RData", nsnip = 0)
execute_queries(file = "cloud_technology_term.RData", nsnip = 0)
execute_queries(file = "cloud_resources_term.RData", nsnip = 0)
execute_queries(file = "cloud_storage_term.RData", nsnip = 0)
execute_queries(file = "software_as_a_service_term.RData", nsnip = 0)
execute_queries(file = "cloud_applications_term.RData", nsnip = 0)

load(file = "cloud_applications_term.RData")

### Combine all hits numbers into one df
load(file = "cloud_computing_term.RData")
hits1 <- hits
colnames(hits1)[which(colnames(hits1) == "count")] = "cloud_computing_count"

load(file = "cloud_technology_term.RData")
hits2 <- hits
colnames(hits2)[which(colnames(hits2) == "count")] = "cloud_technology_count"

load(file = "cloud_resources_term.RData")
hits3 <- hits
colnames(hits3)[which(colnames(hits3) == "count")] = "cloud_resources_count"

load(file = "cloud_storage_term.RData")
hits4 <- hits
colnames(hits4)[which(colnames(hits4) == "count")] = "cloud_storage_count"

load(file = "software_as_a_service_term.RData")
hits5 <- hits
colnames(hits5)[which(colnames(hits5) == "count")] = "software_as_a_service_count"

load(file = "cloud_applications_term.RData")
hits6 <- hits
colnames(hits6)[which(colnames(hits6) == "count")] = "cloud_applications_count"

cloud_term_hits <- left_join(hits1,hits2,by = c("date")) %>%
  left_join(.,hits3,by = c("date")) %>%
  left_join(.,hits4,by = c("date")) %>%
  left_join(.,hits5,by = c("date")) %>%
  left_join(.,hits6,by = c("date"))

cloud_term_hits <- cloud_term_hits %>%
  mutate(total_count = cloud_computing_count + cloud_technology_count + cloud_resources_count + 
           cloud_storage_count + software_as_a_service_count + cloud_applications_count) %>%
  mutate(pct_cloud_computing = cloud_computing_count / total_count) %>%
  mutate(pct_cloud_technology = cloud_technology_count / total_count) %>%
  mutate(pct_cloud_resources = cloud_resources_count / total_count) %>%
  mutate(pct_cloud_storage = cloud_storage_count / total_count) %>%
  mutate(pct_software_as_a_service = software_as_a_service_count / total_count) %>%
  mutate(pct_cloud_applications = cloud_applications_count / total_count)

#Proportion of Cloud Entries
ggplot(cloud_term_hits) +
  geom_line(aes(x = date, y = pct_cloud_computing, col = "Cloud Computing"), size = 0.5) + 
  geom_line(aes(x = date, y = pct_cloud_technology, col = "Cloud Technology"), size = 0.5) + 
  geom_line(aes(x = date, y = pct_cloud_resources, col = "Cloud Resources"), size = 0.5) + 
  geom_line(aes(x = date, y = pct_cloud_storage, col = "Cloud Storage"), size = 0.5) + 
  geom_line(aes(x = date, y = pct_software_as_a_service, col = "Software as a Service"), size = 0.5) + 
  geom_line(aes(x = date, y = pct_cloud_applications, col = "Cloud Applications"), size = 0.5) + 
  scale_colour_manual("",
                      breaks = c("Cloud Computing", "Cloud Technology", "Cloud Resources", 
                                 "Cloud Storage", "Software as a Service", "Cloud Applications"),
                      values=c("red","blue","dark green", "orange", "purple", "brown")) + 
  xlab("Date") + 
  ylab("Proportion") + 
  labs(title = "Proportion of Cloud Computing Query Monthly Hits across Terms")

#Total Number of Entries
ggplot(cloud_term_hits) +
  geom_line(aes(x = date, y = cloud_computing_count, col = "Cloud Computing"), size = 0.5) + 
  geom_line(aes(x = date, y = cloud_technology_count, col = "Cloud Technology"), size = 0.5) + 
  geom_line(aes(x = date, y = cloud_resources_count, col = "Cloud Resources"), size = 0.5) + 
  geom_line(aes(x = date, y = cloud_storage_count,  col = "Cloud Storage"), size = 0.5) + 
  geom_line(aes(x = date, y = software_as_a_service_count, col = "Software as a Service"), size = 0.5) + 
  geom_line(aes(x = date, y = cloud_applications_count, col = "Cloud Applications"), size = 0.5) + 
  geom_line(aes(x = date, y = total_count, col = "Total"), size = 0.5) + 
  scale_colour_manual("",
                      breaks = c("Cloud Computing", "Cloud Technology", "Cloud Resources", 
                                 "Cloud Storage", "Software as a Service", "Cloud Applications", 
                                 "Total"),
                      values=c("red","blue","dark green", "orange", "purple", "brown", "black")) + 
  xlab("Date") + 
  ylab("Number of Hits") + 
  labs(title = "Total Cloud Computing Query Monthly Hits across Terms")

write_csv(cloud_term_hits, "~/Desktop/Research/cloud_term_hits.csv")

remDr$close()