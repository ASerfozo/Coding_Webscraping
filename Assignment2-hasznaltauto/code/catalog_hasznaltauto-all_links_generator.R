##########################################
##       Coding 2 - Assignment 2        ##
##          Attila Serfozo              ##
##             Get all link             ##
##########################################

# Packages
rm(list=ls())
#library(data.table)
library(rvest)
#library(tidyverse)

data_out <- "C:/Users/Attila/Documents/CEU/Coding_Webscraping/Assignment2-hasznaltauto/"

# Getting the list of available brands on site ----------------------------

main_page <- 'https://katalogus.hasznaltauto.hu/'
t <- read_html(main_page)
brands <-
  t %>% 
  html_nodes('.nemlatszik') %>%
  html_text()

# Cleaning brands
brands <- brands[2]
brands <- gsub(' ', '_', trimws(gsub('\n', ',', trimws(gsub('\t', '', trimws(brands), fixed = T)), fixed = T)), fixed = T)
brands <- strsplit(brands,",")[[1]]
brands <- tolower(brands)


# Generating site links ---------------------------------------------------

get_car_links <- function(brand) {
  #brand <- 'volvo'
  
  # Find last page of the brand car list
  x <- read_html(paste0('https://katalogus.hasznaltauto.hu/',brand))
  last_page <-
    x %>% 
    html_nodes('.utolso a') %>%
    html_attr('href')
  
  if(length(last_page) == 0) { # If there is no jump to last page button ~ try maserati
    if (length(x %>% 
               html_nodes('.oldalszamozas .oldalszam') %>%
               html_text()) == 0) {   # If there is only 1 page ~ try bentley
      last_page <- 1
    }else{
      last_page <-
        x %>% 
        html_nodes('.oldalszamozas .oldalszam') %>%
        html_text()
      last_page <- as.numeric(tail(last_page, n=1))
    }
  }else{                       # If there is a jump to last page button ~ try honda
    last_page <- as.numeric(gsub("[^0-9]","",last_page ) )
  }
  
  # Generate pages and car links
  pages <- paste0('https://katalogus.hasznaltauto.hu/',brand,'/page', 1:last_page)
  #return(pages)
  
  car_links <- NULL
  for (i in 1:length(pages)) {
    
    p <- read_html(pages[i])
    
    car_links <- c(car_links,
                   p %>% 
                     html_nodes('.hasonlit-jelolgeto a') %>%
                     html_attr('href')
    )
  }
  
  return(car_links)
}

# Gather all links on site
all_link <- NULL
for (i in 2:length(brands)) {
  all_link <- c(all_link,get_car_links(brands[i]))
}

# Write out results to a csv
write.csv(all_link,paste0(data_out,"data/all_catalog_links.csv"))
