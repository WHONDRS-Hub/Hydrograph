#load libraries
require(neonUtilities)
require(lubridate)

#set working directory 
setwd('C:\\Users\\mcgr323\\projects\\Hydrograph')
#('//pnl/projects/SBR_SFA/Campaign C/Hydropeaking_Network/WHONDRS_and_Non-WHONDRS_Data/03_Metadata')

#download field sites csv from(https://www.neonscience.org/field-sites/field-sites-map
#load in field site csv 
NEON_Field_Sites = read.csv("field-sites.csv")

#load metadata file for WHONDRS data and isolate NEON 4-digit Site Code.
metadata = read.csv("WHONDRS_S19S_Metadata_v3.csv")
NEON_data = as.data.frame(metadata[grep("NEON", metadata$Associated_Site), ])
NEON_data$Associated_Site = as.character(NEON_data$Associated_Site)

NEON_data$site = gsub("National Ecological Observatory ", "", NEON_data[,'Associated_Site'])
NEON_data$site = gsub("\\s*\\([^\\)]+\\)","", NEON_data[,'site'])
NEON_data$site = gsub("site: ", "", NEON_data[,'site'])
NEON_data$site = gsub(" ", "", NEON_data[,'site'])
NEON_data$site_no = gsub("S19S_", "", NEON_data[,'Sample_ID'])

#set outputs folder filepath
Outpath = "C:\\Users\\mcgr323\\projects\\Hydrograph\\outputs"
for (i in 1: length(NEON_data$site)) {

  dpID1 = "DP4.00130.001" #stream discharge
  dpID2 = "DP1.20016.001" #gauge height

  # Define site ID for each location:
  siteID = NEON_data$site[12]

  # Define the startdate and enddate values for each location (1 year prior and 1 month post-sampling):
  sampledate = as.POSIXlt(NEON_data$Date[12], tz = "UTC", format = "%d-%b-%y")
  startdate = format(floor_date((ymd(as.Date(sampledate, format = "%Y-%m-%d"))-years(1)), unit = "months"), "%Y-%m")
  enddate = format(floor_date((ymd(as.Date(sampledate, format = "%Y-%m-%d"))+months(1)), unit = "months"), "%Y-%m")

#download data from NEON database
load.error = tryCatch(loadByProduct(dpID = dpID1,
                     site = c(siteID),
                     startdate = (startdate) ,
                     enddate = (enddate) ,
                     package = "basic",
                     check.size = F,
                     nCores = 1,
                     forceParallel = F,
                     token = NA), error = function(e) e)

if(is(load.error, "error")){
  file = loadByProduct(dpID = dpID2,
                       site = c(siteID),
                       startdate = (startdate) ,
                       enddate = (enddate) ,
                       package = "basic",
                       check.size = F,
                       nCores = 1,
                       forceParallel = F,
                       token = NA)
      data = file$EOS_5_min
      data$agencycd = "NEON"
      data$site_no = NEON_data$site[12]
      names(data)[11] = "datetime"
      data$datetime = format(data$datetime, format="%m/%d/%Y %H:%M" )
      data$tz_cd = "CST"
      data$Discharge_cubic_m_per_second = "Not_Provided"
      names(data)[13] = "Gauge_height_m"
      dpID = dpID2
} else {
  file = load.error
  data = file$csd_continuousDischarge
  data$agencycd = "NEON"
  data$site_no = NEON_data$site[12]
  names(data)[4] = "datetime"
  data$datetime = format(data$datetime, format="%m/%d/%Y %H:%M")
  data$tz_cd = "CST"
  names(data)[11] = "Discharge_cubic_m_per_second"
  data$Discharge_cubic_m_per_second = (data$Discharge_cubic_m_per_second/1000)
  names(data)[7] = "Gauge_height_m"
  dpID = dpID1
}

  #generate data frame with necessary information
  df = data.frame (agencycd = data[,"agencycd"], 
                   site_no = data[,"site_no"], 
                   datetime = data[,"datetime"], 
                   tz_cd = data[,"tz_cd"], 
                   Discharge_cubic_m_per_second = data[,"Discharge_cubic_m_per_second"], 
                   Gauge_height_m = data[,"Gauge_height_m"])

  #replace "NA" with "Not_Provided"
  df$Gauge_height_m[is.na(df$Gauge_height_m)]="Not_Provided"
  df$Discharge_cubic_m_per_second[is.na(df$Discharge_cubic_m_per_second)]="Not_Provided"

  #add reference data
  Line_1 = paste("National Ecological Observatory Network. ", 
                 format(Sys.Date(),"%Y"),". Data Product: ",dpID,". 
                 Provisional data downloaded from http://data.neonscience.org on ",
                 Sys.Date(),". Battelle, Boulder, CO, USA")
  
  Line_2 = paste ("NEON site: ", NEON_data$site[12], "-", 
                  NEON_Field_Sites$Site.Name[which(grepl(x = NEON_Field_Sites$Site.ID, pattern = NEON_data$site[12]))], 
                  ",", NEON_Field_Sites$State[which(grepl(x = NEON_Field_Sites$Site.ID, pattern = NEON_data$site[12]))])

  reference = rbind(gsub(',',' ',Line_1),gsub(',',' ',Line_2))


  #write csv 
  write.csv(x = df, file = file.path(Outpath,paste0('WHONDRS_S19S_Hydrograph_NEON_',
                                      NEON_data$site_no[which(grepl(x = NEON_data$site, pattern = NEON_data$site[12]))],'.csv')), 
                                      row.names = FALSE)

  Out_file = file(file.path(Outpath,paste0('WHONDRS_S19S_Hydrograph_NEON_', 
                                           NEON_data$site_no[which(grepl(x = NEON_data$site, 
                                           pattern = NEON_data$site[12]))],'.csv')),open = "wt")

  writeLines(reference,Out_file)

  write.csv(df,Out_file,row.names = F)

  close(Out_file)
  load.error = ""
}
