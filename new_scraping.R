library(tidyverse)
library(readr)
library(RSelenium)
library(rvest) # We should consider using this in lieu of RSelenium for the queries
library(stringr) # We use str_split() and and str_replace() in get_number()
library(profvis) # We use pause()
library(zoo)
library(ggplot2)

test_url <- "https://infoweb-newsbank-com.stanford.idm.oclc.org/resources/search/nb?p=AWNB&b=results&action=search&fld0=YMD_date&val0=Jan+1980&bln1=AND&fld1=YMD_date&val1=&sort=YMD_date%3AD"
driver <- rsDriver()
remDr <- driver[["client"]]
remDr$navigate(test_url)

url <- "https://infoweb.newsbank.com/apps/news/results?p=AWNB&t=&sort=_rank_%3AD&maxresults=20&f=advanced&b=results&val-base-0=1980-2015&fld-base-0=YMD_date&bln-base-1=and&val-base-1=body&fld-base-1=alltext"
remDr$navigate(url)

## Find number of hits
text_line <- remDr$findElement(using = 'css selector', ".search-hits__meta--total_hits")
text_num <- as.character(text_line$getElementText())

num <- unlist(str_split(text_num, " "))[1]
num <- as.numeric(str_remove_all(num, ","))
num

## Pull snippets
css <- remDr$findElement(using = 'css selector', ".preview-dynamic")
snippet <- as.character(css$getElementText())



cap <- 10
for(j in 1:cap) {
  #print(j)
  #css <- paste("li:nth-child(", j, ") .preview", sep = "")
  css <- paste("search-hits__hit--", j, ".preview-dynamic", sep = "")
  text_box <- remDr$findElement(using = 'css selector', css)
  text_snippet <- as.character(text_box$getElementText())
  #print(text_snippet)
  snippets[count] <- text_snippet
  count <- count + 1
  #delay_time <- abs(rnorm(1,3,0.5))
  #print(delay_time)
  #pause(delay_time)
}

remDr$close()

