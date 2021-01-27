#load libraries 
require(readxl)
require(tidyverse)
require(dplyr)
require(arsenal)

###load NEON data###
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\NEON')#set working dir
file_list <- list.files(path='C:\\Users\\mcgr323\\projects\\Hydrograph\\NEON') #create list of files
dataset_NEON <- data.frame() #create empty data frame for for loop

for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], skip = 2) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$agency_cd <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[4]})#create agency column 
  temp_data$site <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[5]}) #creating a new column that indicates site
  dataset_NEON <- rbind(dataset_NEON, temp_data) #for each iteration, bind the new data to the building dataset
}
dataset_NEON <- within(dataset_NEON, rm(agencycd)) #remove "agencycd" duplicate field

###load USGS data###
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\USGS')#set working dir
file_list <- list.files(path='C:\\Users\\mcgr323\\projects\\Hydrograph\\USGS') #create list of files
dataset_USGS <- data.frame() #create empty data frame for for loop

for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], skip = 2) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$site <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[5]}) #creating a new column that indicates site
  dataset_USGS <- rbind(dataset_USGS, temp_data) #for each iteration, bind the new data to the building dataset
}

###load CANADA water office data####
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA')#set working dir
file_list <- list.files(path='C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA') #create list of files
dataset_CAN <- data.frame() #create empty data frame for for loop

for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], skip = 10) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$agency_cd <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[4]}) #create agency column 
  temp_data$site <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[5]}) #creating a new column that indicates site
  dataset_CAN <- rbind(dataset_CAN, temp_data) #for each iteration, bind the new data to the building dataset
}

dataset_CAN <- within(dataset_CAN, rm(Parameter)) #remove "Parameter" field
dataset_CAN <- dataset_CAN %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "Value (m3/s)",
         "datetime"= "Date (EST)")
#dataset_CAN$datetime = format(dataset_CAN$datetime, format="%m/%d/%Y %H:%M" )
         
dataset_CAN$site_no <- NA #site number (not S19S number)
dataset_CAN$tz_cd <- "EST" #time zone 
dataset_CAN$Gauge_height_m <- "Not Provided" #gauge height

###bind the dataframes###
hydrograph_data <- rbind(dataset_NEON,dataset_USGS,dataset_CAN)

