##########################################
##       Coding 2 - Assignment 2        ##
##          Attila Serfozo              ##
##             Cleaning                 ##
##########################################

# Packages
rm(list=ls())
library(data.table)
library(rvest)
library(tidyverse)

#########################################
# Please add selected brand to be cleaned
selected_brand <- 'bentley'
#########################################

data_in <- "C:/Users/Attila/Documents/CEU/Coding_Webscraping/Assignment2-hasznaltauto/data/"
car_data <- read.csv(paste0(data_in,"raw/",selected_brand,"-catalog.csv"))

car_data_clean <- separate(car_data, Maximális.teljesítmény, "," ,
                    into = c('Maximális.teljesítmény.kW', 'Maximális.teljesítmény.LE'))
car_data_clean <- 
  car_data_clean %>%
  mutate ( "Gyorsulás.0.ról.100.km.h.ra" = gsub(",", ".", car_data_clean$Gyorsulás.0.ról.100.km.h.ra),
           "Vegyes.fogyasztás" = gsub(",", ".", car_data_clean$Vegyes.fogyasztás))

car_data_clean <- 
  car_data_clean %>% 
  transmute(
    'ID' = X,
    'Név' = Név,
    'Link' = Link,
    'Kategória' = Kategória,
    'Kivitel' = Kivitel,
    #'Gyártási.idoszak' = Gyártási.időszak,
    'Üzemanyag' = Üzemanyag,
    'Újkori.ára.(mHUF)' = as.numeric(gsub("[^0-9]","",Újkori.ára) )/1000000,
    'Hengerek.száma' = as.numeric(gsub("[^0-9]","",Hengerek.száma) ),
    'Maximális.teljesítmény.(kW)' = as.numeric(gsub("[^0-9]","",Maximális.teljesítmény.kW) ),
    'Maximális.teljesítmény.(LE)' = as.numeric(gsub("[^0-9]","",Maximális.teljesítmény.LE) ),
    'Végsebesség.(km/h)' = as.numeric(gsub("[^0-9]","",Végsebesség) ),
    'Gyorsulás.(s)' = as.numeric(gsub("[^0-9\\.]","",car_data_clean$Gyorsulás.0.ról.100.km.h.ra) ),
    'CO2.kibocsátás.(g/km)' = as.numeric(gsub("[^0-9]","",CO2.kibocsátás) ),
    'Vegyes.fogyasztás.(l)' = as.numeric(gsub("[^0-9\\.]","",Vegyes.fogyasztás) ),
    'Össztömeg.(kg)' = as.numeric(gsub("[^0-9]","",Össztömeg) ),
    'Szélesség.(mm)' = as.numeric(gsub("[^0-9]","",Szélesség) ),
    'Hosszúság.(mm)' = as.numeric(gsub("[^0-9]","",Hosszúság) ),
    'Magasság.(mm)' = as.numeric(gsub("[^0-9]","",Magasság) ),
    'Ajtók.száma' = as.numeric(gsub("[^0-9]","",Ajtók.száma) ),
    'Szállítható.személyek.száma' = as.numeric(gsub("[^0-9]","",Szállítható.személyek.száma) )
  )

data_out <- "C:/Users/Attila/Documents/CEU/Coding_Webscraping/Assignment2-hasznaltauto/data/"
write.csv(car_data_clean,paste0(data_out,"clean/",selected_brand,"-catalog.csv"))












