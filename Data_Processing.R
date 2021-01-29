#load libraries 
require(readxl)
require(tidyverse)
require(dplyr)
require(lubridate)

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

#change date time format to UTC
dataset_NEON$datetime <- as.POSIXct(strptime(dataset_NEON$datetime, "%m/%d/%Y %H:%M", tz = "ETC/GMT+6"))
attr(dataset_NEON$datetime,"tzone") <- "America/Chicago"
dataset_NEON$datetime <- with_tz(dataset_NEON$datetime, "UTC")


###load USGS data###
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\USGS')#set working dir
file_list <- list.files(path='C:\\Users\\mcgr323\\projects\\Hydrograph\\USGS') #create list of files
dataset_USGS <- data.frame() #create empty data frame for for loop

for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], skip = 2) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$site <- sapply(strsplit(gsub("..csv", "", file_list[i]), "_"), function(x){x[5]}) #creating a new column that indicates site
  dataset_USGS <- rbind(dataset_USGS, temp_data) #for each iteration, bind the new data to the building dataset
}

#change date time format to UTC
dataset_USGS$datetime <- as.POSIXct(strptime(dataset_USGS$datetime, "%d %b %Y %H:%M", tz = "ETC/GMT+5"))
attr(dataset_USGS$datetime,"tzone") <- "America/New_York"
dataset_USGS$datetime <- with_tz(dataset_USGS$datetime, "UTC")

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

#change date time format to UTC
dataset_CAN <- within(dataset_CAN, rm(Parameter)) #remove "Parameter" field
dataset_CAN <- dataset_CAN %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "Value (m3/s)",
         "datetime"= "Date (EST)")
         
dataset_CAN$site_no <- NA #site number (not S19S number)
dataset_CAN$tz_cd <- "EST" #time zone 
dataset_CAN$Gauge_height_m <- "Not Provided" #gauge height

#change date time format to UTC
dataset_CAN$datetime <- as.POSIXct(strptime(dataset_CAN$datetime, "%Y-%m-%d %H:%M:%S", tz = "ETC/GMT+5"))
attr(dataset_CAN$datetime,"tzone") <- "America/New_York"
dataset_CAN$datetime <- with_tz(dataset_CAN$datetime, "UTC")

###read in other files###
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\Other')
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

valid_fields <- c("DIS", "dis", "DAT", "dat", "GAGE", "gage","GAUG", "Q.cms", "Staff") #valid fields 

#data from hydroshare (can be used for other hydroshare files)
ORNL_0039 <- WHONDRS_S19S_Hydrograph_ORNL_0039_02 %>% select(contains(valid_fields))
ORNL_0039$site <- 0039
ORNL_0039$agency_cd <- "ORNL"
ORNL_0039$tz_cd <- "EDT"

OWRD_0041 <- WHONDRS_S19S_Hydrograph_OWRD_0041 %>% select(contains(valid_fields))
OWRD_0041$site <- 0041
OWRD_0041$agency_cd <- "OWRD"
OWRD_0041$tz_cd <- "PDT"

dataset_other <- rbind(ORNL_0039, OWRD_0041)
dataset_other <- dataset_other %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "Q.cms",
         "datetime"= "DateTime",
         "Gauge_height_m" = "Staff.ft")

BCCZO_0071 <- WHONDRS_S19S_Hydrograph_BCCZO_0071 %>% select(contains(valid_fields)) #select valid fields for S19S0071
BCCZO_0071$site <- 0071
BCCZO_0071$agency_cd <- "BCCZO"
BCCZO_0071$tz_cd <- "MST"
BCCZO_0071 <- BCCZO_0071 %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "DISCHARGE.m.3.10..3..s",
         "datetime"= "DATE_TIME")
BCCZO_0071$Gauge_height_m <- "Not Provided"


CODWR_valid_fields <- c("DISCHRG.Value", "Date.Time", "GAGE_HT.Value")
CODWR_0077 <- WHONDRS_S19S_Hydrograph_CODWR_0077 %>% select(contains(CODWR_valid_fields))
CODWR_0077 <- CODWR_0077 %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "DISCHRG.Value",
         "datetime" = "Date.Time",
         "Gauge_height_m" = "GAGE_HT.Value")
CODWR_0077$site <- 0077
CODWR_0077$agency_cd <- "COWDR"
CODWR_0077$tz_cd <- "MST"
CODWR_0077$datetime <- gsub("=", "", CODWR_0077$datetime)

dataset_other <- rbind(dataset_other, CODWR_0077, BCCZO_0071)
dataset_other$site_no <- NA

###bind the dataframes###
hydrograph_data <- as.data.frame(rbind(dataset_NEON,dataset_USGS,dataset_CAN))
hydrograph_data <- within(hydrograph_data, rm(tz_cd))

#change classes to get mean daily discharge
hydrograph_data$site  <- as.factor(hydrograph_data$site)
hydrograph_data$Discharge_cubic_m_per_second <- as.numeric(hydrograph_data$Discharge_cubic_m_per_second)

#remove NAs 
hydrograph_data <- na.omit(hydrograph_data)

#create new dataframe of daily discharge data
discharge_data <- hydrograph_data %>%
                              mutate(datetime = floor_date(datetime, "day")) %>%
                              group_by(datetime, site) %>%
                              summarize(mean_daily_discharge = mean(Discharge_cubic_m_per_second, 
                                                                    na.rm=TRUE),
                                                                    .groups = 'drop')


#write out as csv
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph')
write.csv(discharge_data, "S191S_daily_dischrg_data.csv")
