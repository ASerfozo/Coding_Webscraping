##########################################
##       Coding 2 - Assignment 2        ##
##          Attila Serfozo              ##
##             Get data                 ##
##########################################

# Packages
rm(list=ls())
library(data.table)
library(rvest)
library(tidyverse)

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
brands <- brands[2:107]

write.csv(brands,paste0(data_out,"data/available_brands.csv"))

# Get one car data --------------------------------------------------------

my_link <- 'https://katalogus.hasznaltauto.hu/abarth/124_spider_1.4_multiair_t-107786'
get_one_car <- function(my_link) {
  
  t2 <- read_html(my_link)
  
  specification_list <- list()
  
  
  titles <-
    t2 %>% 
    html_nodes('h1') %>%
    html_text()
  
  link <- my_link
  
  specification_list [['Név']] <- titles
  specification_list [['Link']] <- link
  
  
  category <- t2 %>%  html_nodes('.w250') %>%  html_text()
  values <- t2 %>% html_nodes('.w250+ td') %>%  html_text()
  
  if (length(category) == length(values)) {
    for (key_id in 1:length(values)) {
      #print(key_id) 
      specification_list[[ category[key_id] ]] <- values[key_id]
    }
    
  }
  return(specification_list)
}

# Generate pages and data ------------------------------------------------------

get_car_data <- function(brand) {
  #brand <- 'bentley'
  
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
  
  # Generate pagesand car links
  pages <- paste0('https://katalogus.hasznaltauto.hu/',brand,'/page', 1:last_page)
  
  car_links <- NULL
  for (i in 1:length(pages)) {
    
    p <- read_html(pages[i])
    
    car_links <- c(car_links,
                   p %>% 
                     html_nodes('.hasonlit-jelolgeto a') %>%
                     html_attr('href')
    )
  }
  
  df <- rbindlist(lapply(car_links, get_one_car), fill = T)
  return(df)
  
}

# Try out function
car_data1 <- get_car_data('bentley')
write.csv(car_data1,paste0(data_out,"data/raw/bentley-catalog.csv"))

car_data2 <- get_car_data('maserati')
write.csv(car_data2,paste0(data_out,"data/raw/maserati-catalog.csv"))

car_data3 <- get_car_data('alfa_romeo')
write.csv(car_data3,paste0(data_out,"data/raw/alfa_romeo-catalog.csv"))

car_data4 <- get_car_data('honda')
write.csv(car_data4,paste0(data_out,"data/raw/honda-catalog.csv"))

		

