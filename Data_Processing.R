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

###load CANADA water office discharge data####
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA\\discharge')#set working dir
file_list <- list.files(path='C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA\\discharge') #create list of files
discharge_CAN <- data.frame() #create empty data frame for for loop

for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], skip = 10) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$agency_cd <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[4]}) #create agency column 
  temp_data$site <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[5]}) #creating a new column that indicates site
  discharge_CAN <- rbind(discharge_CAN, temp_data) #for each iteration, bind the new data to the building dataset
}

#change date time format to UTC
discharge_CAN <- within(discharge_CAN, rm(Parameter)) #remove "Parameter" field
discharge_CAN <- discharge_CAN %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "Value (m3/s)",
         "datetime"= "Date (EST)")
         
discharge_CAN$site_no <- NA #site number (not S19S number)
discharge_CAN$tz_cd <- "EST" #time zone 

#change date time format to UTC
discharge_CAN$datetime <- as.POSIXct(strptime(discharge_CAN$datetime, "%Y-%m-%d %H:%M:%S", tz = "ETC/GMT+5"))
attr(discharge_CAN$datetime,"tzone") <- "America/New_York"
discharge_CAN$datetime <- with_tz(discharge_CAN$datetime, "UTC")

###load CANADA water office PST gauge ht data####
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA\\gauge_level\\PST')#set working dir
file_list <- list.files(path='C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA\\gauge_level\\PST') #create list of files
gauge_CAN <- data.frame() #create empty data frame for for loop

for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], skip = 9) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$agency_cd <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[4]}) #create agency column 
  temp_data$site <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[5]}) #creating a new column that indicates site
  gauge_CAN <- rbind(gauge_CAN, temp_data) #for each iteration, bind the new data to the building dataset
}

#change date time format to UTC
gauge_CAN <- within(gauge_CAN, rm(Parameter)) #remove "Parameter" field
gauge_CAN <- gauge_CAN %>% #rename columns to match NEON and USGS format 
  rename("Gauge_height_m" = "Value (m)",
         "datetime"= "Date (PST)")

gauge_CAN$site_no <- NA #site number (not S19S number)
gauge_CAN$tz_cd <- "PST" #time zone 

#change date time format to UTC
gauge_CAN$datetime <- as.POSIXct(strptime(gauge_CAN$datetime, "%m/%d/%Y %H:%M", tz = "ETC/GMT+8"))
attr(gauge_CAN$datetime,"tzone") <- "America/San_Francisco"
gauge_CAN$datetime <- with_tz(gauge_CAN$datetime, "UTC")

gauge_CAN_PST <- gauge_CAN

###load CANADA water office CST gauge ht data####
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA\\gauge_level\\CST')#set working dir
file_list <- list.files(path='C:\\Users\\mcgr323\\projects\\Hydrograph\\water_office_CANADA\\gauge_level\\CST') #create list of files
gauge_CAN <- data.frame() #create empty data frame for for loop

for (i in 1:length(file_list)){
  temp_data <- read_csv(file_list[i], skip = 9) #each file will be read in, specify which columns you need read in to avoid any errors
  temp_data$agency_cd <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[4]}) #create agency column 
  temp_data$site <- sapply(strsplit(gsub(".csv", "", file_list[i]), "_"), function(x){x[5]}) #creating a new column that indicates site
  gauge_CAN <- rbind(gauge_CAN, temp_data) #for each iteration, bind the new data to the building dataset
}

#change date time format to UTC
gauge_CAN <- within(gauge_CAN, rm(Parameter)) #remove "Parameter" field
gauge_CAN <- gauge_CAN %>% #rename columns to match NEON and USGS format 
  rename("Gauge_height_m" = "Value (m)",
         "datetime"= "Date (CST)")

gauge_CAN$site_no <- NA #site number (not S19S number)
gauge_CAN$tz_cd <- "CST" #time zone 

#change date time format to UTC
gauge_CAN$datetime <- as.POSIXct(strptime(gauge_CAN$datetime, "%Y-%m-%d %H:%M:%S", tz = "ETC/GMT+7"))
attr(gauge_CAN$datetime,"tzone") <- "America/Denver"
gauge_CAN$datetime <- with_tz(gauge_CAN$datetime, "UTC")

gauge_CAN_CST <- gauge_CAN

gauge_CAN <- rbind(gauge_CAN_CST, gauge_CAN_PST)

###read in other files###
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph\\Other')
temp = list.files(pattern="*.csv")
list2env(
  lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), 
         read.csv), envir = .GlobalEnv)

valid_fields <- c("DIS", "dis", "DAT", "dat", "GAGE", "gage","GAUG", "Q.cms", "Staff") #valid fields 

#data from hydroshare (can be used for other hydroshare files)
###Site 0039###
ORNL_0039 <- WHONDRS_S19S_Hydrograph_ORNL_0039_02 %>% select(contains(valid_fields))
ORNL_0039$site <- "0039"
ORNL_0039$agency_cd <- "ORNL"
ORNL_0039$tz_cd <- "EDT"
ORNL_0039 <- ORNL_0039 %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "Q.cms",
         "datetime"= "DateTime",
         "Gauge_height_m" = "Staff.ft")
ORNL_0039$datetime <- as.POSIXct(strptime(ORNL_0039$datetime, "%Y-%m-%d %H:%M", tz = "ETC/GMT+5")) #change date time format to UTC
attr(ORNL_0039$datetime,"tzone") <- "America/New_York"
ORNL_0039$datetime <- with_tz(ORNL_0039$datetime, "UTC")

###Site 0041###
OWRD_0041 <- WHONDRS_S19S_Hydrograph_OWRD_0041 %>% select(contains(valid_fields))
OWRD_0041$site <- "0041"
OWRD_0041$agency_cd <- "OWRD"
OWRD_0041$tz_cd <- "PDT"
OWRD_0041 <- OWRD_0041 %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "Q.cms",
         "datetime"= "DateTime",
         "Gauge_height_m" = "Staff.ft")
OWRD_0041$datetime <- as.POSIXct(strptime(OWRD_0041$datetime, "%Y-%m-%d %H:%M", tz = "ETC/GMT+8")) #change date time format to UTC
attr(OWRD_0041$datetime,"tzone") <- "America/San_Francisco"
OWRD_0041$datetime <- with_tz(OWRD_0041$datetime, "UTC")


###BC CZO files (can be used for other BC CZO files in the same format)###
BCCZO_0071 <- WHONDRS_S19S_Hydrograph_BCCZO_0071 %>% select(contains(valid_fields)) #select valid fields
BCCZO_0071$site <- "0071"
BCCZO_0071$agency_cd <- "BCCZO"
BCCZO_0071$tz_cd <- "MST"
BCCZO_0071 <- BCCZO_0071 %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "DISCHARGE.m.3.10..3..s",
         "datetime"= "DATE_TIME")
BCCZO_0071$Gauge_height_m <- "Not Provided"
BCCZO_0071$datetime <- as.POSIXct(strptime(BCCZO_0071$datetime, "%m/%d/%y %H:%M", tz = "ETC/GMT+7")) #change date time format to UTC
attr(BCCZO_0071$datetime,"tzone") <- "America/Denver"
BCCZO_0071$datetime <- with_tz(BCCZO_0071$datetime, "UTC")


CODWR_valid_fields <- c("DISCHRG.Value", "Date.Time", "GAGE_HT.Value")
CODWR_0077 <- WHONDRS_S19S_Hydrograph_CODWR_0077 %>% select(contains(CODWR_valid_fields))
CODWR_0077 <- CODWR_0077 %>% #rename columns to match NEON and USGS format 
  rename("Discharge_cubic_m_per_second" = "DISCHRG.Value",
         "datetime" = "Date.Time",
         "Gauge_height_m" = "GAGE_HT.Value")
CODWR_0077$site <- "0077"
CODWR_0077$agency_cd <- "COWDR"
CODWR_0077$tz_cd <- "MST"
CODWR_0077$datetime <- gsub("=", "", CODWR_0077$datetime)
CODWR_0077$datetime <- gsub('"', '', CODWR_0077$datetime)
CODWR_0077$datetime <- as.POSIXct(strptime(CODWR_0077$datetime, "%m/%d/%Y %H:%M", tz = "ETC/GMT+7")) #change date time format to UTC
attr(CODWR_0077$datetime,"tzone") <- "America/Denver"
CODWR_0077$datetime <- with_tz(CODWR_0077$datetime, "UTC")

dataset_other <- rbind(ORNL_0039,
                       OWRD_0041, 
                       CODWR_0077, 
                       BCCZO_0071)
dataset_other$site_no <- NA

###bind the dataframes###
hydrograph_data <- as.data.frame(rbind(dataset_NEON,dataset_USGS,dataset_CAN, dataset_other))
hydrograph_data <- within(hydrograph_data, rm(tz_cd))
length(unique(hydrograph_data$site))

#change classes to summarize 
hydrograph_data$Discharge_cubic_m_per_second <- as.numeric(hydrograph_data$Discharge_cubic_m_per_second)
hydrograph_data$Gauge_height_m <- as.numeric(hydrograph_data$Gauge_height_m)
dis_na <- aggregate(Discharge_cubic_m_per_second ~ site, data=hydrograph_data, function(x) {sum(is.na(x))}, na.action = NULL)
gaug_na <- aggregate(Gauge_height_m ~ site, data=hydrograph_data, function(x) {sum(is.na(x))}, na.action = NULL)

#remove NAs 
hydrograph_data <- na.omit(hydrograph_data)

#create new dataframe of daily discharge data
discharge_data <- hydrograph_data %>%
                              mutate(datetime = floor_date(datetime, "day")) %>%
                              group_by(site, datetime) %>%
                              summarize(mean_daily_discharge = mean(Discharge_cubic_m_per_second, 
                                                                    na.rm=TRUE),
                                                                    .groups = 'drop')

#create new dataframe of daily discharge data
pressure_data <- hydrograph_data %>%
                              mutate(datetime = floor_date(datetime, "day")) %>%
                              group_by(site, datetime) %>%
                              summarize(mean_gauge_ht = mean(Gauge_height_m, 
                                                                    na.rm=TRUE),
                                                                    .groups = 'drop')


#write out as csv
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph')
write.csv(discharge_data, "S191S_daily_dischrg_data.csv")
write.csv(pressure_data, "S191S_daily_gauge_ht.csv")
