################################################################################
# This R code is for downloading daily HMS smoke shape files and creating 
# HMS smoke data at the EPA AQS monitor location (lat, long).
# The script contains the following three parts:
  
# A. Downloading HMS smoke layer shape file
# B. Getting AQS data through RAQSAPI
# C. Creating HMS smoke timeseries at AQS monitor location

# Update History:
# Initial version: Developed by Tsengel Nergui, PhD, LADCO Atmospheric Modeler, April 2024 
# The version was based on LADCO EE screening scripts developed in Aug 2018.

################################################################################


# Libraries ---------------------------------------------------------------

library(STAT)
library(plyr)  # dplyr must be called before dplyr
library(dplyr) # data manipulation
library(ggplot2) # plotting
library(ggrepel) # better label handling
library(data.table) # faster handling a large files


# setting work dir ---
getwd()
setwd("D:/My R workplace/ExceptionalEvent")

#vvv----------------------------------------------------------------------------
# A. Downloading HMS smoke layer shape file 
#-------------------------------------------------------------------------------
# get today's date
rm(proc_date,proc_date1,proc_year)
today <- Sys.time();today
library(lubridate)
#forcing the today's date as in GMT as the HMS smoke dates are in GMT.
today <- force_tz(today, "GMT");str(today)
proc_date <- format(today, tz="GMT", "%b%d");proc_date
proc_date1 <- format(today,tz="GMT", "%Y-%m-%d");proc_date1
proc_year <- as.integer(format(today, tz="GMT", "%Y"));proc_year

# HMS_smk dir where you want to save HMS shape files ---
savedir <- "D:/My R workplace/ExceptionalEvent/HMS_smk"

# define start and end dates for HMS download
start_datetime = as.POSIXct("2024-01-01 00:00:00", tz="GMT");start_datetime
end_datetime = as.POSIXct(proc_date1, tz="GMT");end_datetime
start_datetime;end_datetime

# daily interval date range
rm(drange)
drange = as.character(seq(start_datetime, end_datetime, by="24 hour"))

id = 1
for(idatehh in drange) {
  yyyymmddhh = format(as.POSIXct(idatehh, tz="GMT"), "%Y%m%d%H")
  YYYY = substr(yyyymmddhh, 1,4)
  MM = substr(yyyymmddhh, 5,6)
  YYYYMMDD = substr(yyyymmddhh, 1,8)
  
  print(paste0("downloaded SMK shape file and unzipped for ", yyyymmddhh, " ", id, "/",length(drange)))  
  idate = YYYYMMDD
  #
  file_url <- paste0("https://satepsanone.nesdis.noaa.gov/pub/FIRE/web/HMS/Smoke_Polygons/Shapefile/",YYYY,"/",MM,"/hms_smoke",idate,".zip")
  # Path and file name to save the downloaded file
  save_file <- paste0(savedir, "/HMS_smk_data/hms_smoke",idate,".zip")
  # Use download.file function to download file
  download.file(file_url, save_file)
  file.exists(save_file)
  
  # unzip the file: extract the files to a target directory
  target_dir <- paste0(savedir, "/HMS_smk_data/hms_smoke",idate)
  dir.create(target_dir)
  
  library(utilsIPEA)
  #utils::unzip(save_file, exdir = target_dir)
  unzip(save_file, exdir = target_dir)
  file.remove(save_file)
  id = id + 1
  
}

#^^^----------------------------------------------------------------------------


#vvv----------------------------------------------------------------------------
# B. Getting Daily Data files from AirNow and/or AQS 
#-------------------------------------------------------------------------------
# get today's date
rm(proc_date,proc_date1,proc_year)
today <- Sys.time();today
#forcing the today's date as in GMT
today <- force_tz(today, "GMT");str(today)

proc_date <- format(today, tz="GMT", "%b%d");proc_date
proc_date1 <- format(today,tz="GMT", "%Y-%m-%d");proc_date1
proc_year <- as.integer(format(today, tz="GMT", "%Y"));proc_year

#lets assume daily AQS data as in "GMT" for convenient for date manipulation in R, even though it is in LST
start_datetime = as.POSIXct("2024-04-01 00:00:00", tz="GMT");start_datetime
end_datetime = as.POSIXct(proc_date1, tz="GMT");end_datetime

# daily interval date range
rm(drange)
drange = as.character(seq(start_datetime, end_datetime, by="24 hour"))
start_datetime;end_datetime;drange

# getting airNowTech daily data
rm(get_allairnow_daily)

library(curl)
library(data.table)
get_allairnow_daily = NULL

for(getDD in drange) {
  yyyymmdd = format(as.POSIXct(getDD, tz="GMT"), "%Y%m%d")
  YYYY = substr(yyyymmdd, 1,4)
  YYYYMMDD = substr(yyyymmdd, 1,8)
  #data with lat and long, can be used for subsetting for Midwest
  get_allairnow_daily = rbind(get_allairnow_daily, fread(paste0("https://files.airnowtech.org/airnow/",YYYY, "/", YYYYMMDD, "/daily_data_v2.dat")))
  print(paste0("Downloaded Daily AirNowTech data for ", getDD,"/",end_datetime))

}


dplyr::glimpse(get_allairnow_daily)
names(get_allairnow_daily) = c("valid_date", "site", "sitename", "param","reporting_units","value","averaging_period","data_source","AQI_value","AQI_category",
                               "latitude", "longitude", "full_AQSID")


# data massaging: 
# replacing the date slash to dash to avoid R's "/" used for indicating special chars
get_allairnow_daily$valid_date <- gsub("/", "-", get_allairnow_daily$valid_date) #replaces any slashes with dashes
get_allairnow_daily$date <- as.Date(get_allairnow_daily$valid_date, "%m-%d-%y")
get_allairnow_daily$sitename <- gsub(" $", "", get_allairnow_daily$sitename) #removes any trailing spaces from site names
get_allairnow_daily$sitename <- gsub("/", "-", get_allairnow_daily$sitename) #replaces any slashes with dashes
get_allairnow_daily$sitename <- gsub(",", " ", get_allairnow_daily$sitename) #replaces any comma with space

# getting rid off a dot from param for using the param in output filename 
get_allairnow_daily$param <- gsub("PM2.5-24hr", "PM25-24hr", get_allairnow_daily$param)
unique(get_allairnow_daily$param)
colnames(get_allairnow_daily)

#forcing the date as in GMT
get_allairnow_daily$date <- force_tz(get_allairnow_daily$date, "GMT")
get_allairnow_daily$country_code <- substr(get_allairnow_daily$full_AQSID,1,3)
get_allairnow_daily$siteID <- substr(get_allairnow_daily$full_AQSID,4,12)
glimpse(get_allairnow_daily)

# define LADCO and beyond extend
ladco.lat <-c(35.0,60.0)
ladco.lon <-c(-100.0,-75.0)

library(dplyr)
rm(airnow_PM_24hravg)
airnow_PM_24hravg <- get_allairnow_daily %>%
  dplyr::filter(param == "PM25-24hr" & averaging_period == 24 &
                  (latitude >= ladco.lat[1] & latitude <= ladco.lat[2]) & (longitude >= ladco.lon[1] & longitude <= ladco.lon[2]) )  %>%
  dplyr::select(param,siteID,longitude,latitude,date,value,full_AQSID)

unique(airnow_PM_24hravg$param)
dim(airnow_PM_24hravg)

# write out data with proc_date
outdir <- "D:/My R workplace/ExceptionalEvent/airnow/Auto_Pull"
proc_year
outfile_DA_PM25 <- paste(outdir,"/AIRNOW_PM25-24hr_",proc_year,".csv",sep="");outfile_DA_PM25
write.table (airnow_PM_24hravg,outfile_DA_PM25,row.names=F,col.names=T,sep=",", quote=FALSE) 
print(paste("Written out AirNow PM25 data",outfile_DA_PM25,sep = " " ))



#vvv----------------------------------------------------------------------------
# D. Create a SMK timeseries to the daily AIRNOW PM25 monitor locations
#-------------------------------------------------------------------------------
library(sf)
savedir <- "D:/My R workplace/ExceptionalEvent/HMS_smk"
outdir <- "D:/My R workplace/ExceptionalEvent/airnow/Auto_Pull"

rm(dat2)
dat2 <- airnow_PM_24hravg
glimpse(dat2)

# Convert date/time columns to date/time objects
library(lubridate)
dat2$DateObj <- parse_date_time(dat2$date, orders = "%Y-%m-%d", tz = "GMT")
# sites with multiple pocs  mostl likely to have the same locations (lat/lon), this created site_date arrays
dat2$site_date <- paste(dat2$siteID,dat2$DateObj,sep = "_")
unique(dat2$site_date)

# create date array
rm(date_array)
date_array <- sort(unique(dat2$DateObj))
#str(date_array)
#tail(date_array)


#---------------------------------------------    
# for today's (measurement date) HMS overhead
#---------------------------------------------    
# initialize dat2_smk1 df 
dat2_smk1 <- NULL
length(date_array)  
for(i in 1:length(date_array))  {
    # select all sites for anal_date ---  
    library(dplyr)
    rm(dat2_idate)
    dat2_idate <- dat2 %>% dplyr::filter(DateObj == date_array[i]) %>% dplyr::select(site_date,longitude,latitude)
    rm(anal_idate,anal_jdate,smk_idate,smk0,smk1,smk1_tmp,pnts_sf,smk1_tmp_sf,tmp0,tmp1,tmp2,tmp3)  
    # HMS data's date_time is in GMT as it is satellite derived data
    #date_array[i] is POSIXct formatted.
    anal_idate <- as.POSIXct(date_array[i], "%Y%j %H%M",tz="GMT") ;anal_idate
    print(paste("---Processing anal_idate ", anal_idate, sep = " " ))  
      
      smk_idate <- paste0(sprintf("%4d",year(anal_idate)),sprintf("%02d",month(anal_idate)),sprintf("%02d",day(anal_idate)));smk_idate
      target_dir <- paste0(savedir,"/HMS_smk_data/hms_smoke",smk_idate)
      print(paste("----------------------------------------------Processing ", i,"/",length(date_array), anal_idate, sep = " " ))
      
      # reading a file with an error handler ---
      skip_to_next <- FALSE
      smk0 <- tryCatch(tendays <- sf::st_read(paste0(target_dir,"/hms_smoke",smk_idate,".shp")),
                       onTimeout = "silent", TimeoutException=function(ex) { "Timedout" },
                       warning=function(w) { "Warnings" } , error=function(e) { skip_to_next <<- TRUE } )
      
      if(skip_to_next) { next }     
      
      # diagnosis for checking if all polygons are valid, FALSE has to be zero
      table(sf::st_is_valid(smk0))
      rm(smk1)
      smk1 <- smk0[sf::st_is_valid(smk0),]
      table(sf::st_is_valid(smk1))
      
      # op2: subset by jdate string ---
      head(smk1)
      smk1$start1 <- substr(smk1$Start, 1, 7)
      smk1$end1 <- substr(smk1$End, 1, 7)
      anal_jdate <- paste0(sprintf("%4d",year(anal_idate)),sprintf("%03d",yday(anal_idate)));anal_jdate
      
      rm(smk1_tmp)
      smk1_tmp <- raster::subset(smk1, start1 == anal_jdate & end1 == anal_jdate )
      head(smk1_tmp)
      
      
      if (length(smk1_tmp) >= 1) {
        # transform to polygon and polygon-sf --- 
        library(sf)
        smk1_tmp_sf <- sf::st_as_sf(smk1_tmp, coords=c("long","lat"), crs = st_crs(smk1))
        
        # transform obs point data to point-sf ---  
        pnts_sf <- sf::st_as_sf(dat2_idate, coords=c("longitude","latitude"), crs = st_crs(smk1))
        # intersect point to polygon, then extract HMS smk Density column
        dim(pnts_sf)
        rm(tmp0)
        tmp0 <- sf::st_intersects(pnts_sf, smk1_tmp_sf,sparse = FALSE)
        # row is site and column is against 9 smk polygons
        head(tmp0)
        
        # start putting SMK info for each monitor that intersects with polygons
        for (j in 1: dim(tmp0)[1]) {
          # check for not found intersect ---
          # (sum(tmp0, na.rm=TRUE) == 0 ) counts only TRUE ---
          if (sum(tmp0[j,], na.rm=TRUE) == 0 ) {
            # no intersect found
            dat2_idate$HMS_smk0[j] <- NA
 
          } else {
            # one or more intersects found
            tmp1 <- smk1_tmp_sf$Density[unlist(tmp0[j,])];tmp1
            # if an intersect found
            if (length(tmp1) == 1) { 
              dat2_idate$HMS_smk0[j] <- tmp1
            } else {
              # else bring the most frequent density category of smoke
              dat2_idate$HMS_smk0[j] <- names(which.max(table(tmp1))) 
                
            }
            print(paste("---Processing site ", j,"/",dim(tmp0)[1], dat2_idate[j,1], sep = " " ))
            
          } # if single or multi intersect are found
          
        } #j for sites
        
      } #if_exist subsetted date smk1
      if (i == 1 ) { dat2_smk1 <- dat2_idate } else { dat2_smk1 <- rbind(dat2_smk1,dat2_idate ) }

} # i for date  

head(dat2_smk1)

#---------------------------------------------    
# for yesterday's (measurement date - 1 day) HMS overhead
#---------------------------------------------  

dat2_smk2 <- NULL
  
for(i in 1:length(date_array))  {
    # finding today and yesterday for assessing the HMS smoke presence for the last 2 days
    rm(yesterday)
    today <- as.POSIXct(date_array[i], "%Y-%m-%d",tz="GMT") ;today
    yesterday <- as.POSIXct(date_array[i]-24*60*60, "%Y-%m-%d",tz="GMT") ;yesterday
    rm(anal_idate,anal_jdate,smk_idate,smk0,smk1,smk1_tmp,pnts_sf,smk1_tmp_sf,tmp0,tmp1,tmp2,tmp3)  
    # HMS data's date_time is in GMT as it is satellite derived data
    #date_array[i] is POSIXct formatted.
    anal_idate <- as.POSIXct(yesterday, "%Y%j %H%M",tz="GMT")
    print(paste("---Processing anal_idate ", anal_idate, sep = " " ))  
    
    smk_idate <- paste0(sprintf("%4d",year(anal_idate)),sprintf("%02d",month(anal_idate)),sprintf("%02d",day(anal_idate)));smk_idate
    target_dir <- paste0(savedir,"/HMS_smk_data/hms_smoke",smk_idate)
    print(paste("----------------------------------------------Processing ", i,"/",length(date_array), anal_idate, sep = " " ))
    
    # reading a file with an error handler ---
    skip_to_next <- FALSE
    smk0 <- tryCatch(tendays <- sf::st_read(paste0(target_dir,"/hms_smoke",smk_idate,".shp")),
                     onTimeout = "silent", TimeoutException=function(ex) { "Timedout" },
                     warning=function(w) { "Warnings" } , error=function(e) { skip_to_next <<- TRUE } )
    
    if(skip_to_next) { next }     
    
    # diagnosis for checking if all polygons are valid, FALSE has to be zero
    table(sf::st_is_valid(smk0))
    rm(smk1)
    smk1 <- smk0[sf::st_is_valid(smk0),]
    table(sf::st_is_valid(smk1))
    
    # op2: subset by jdate string ---
    head(smk1)
    smk1$start1 <- substr(smk1$Start, 1, 7)
    smk1$end1 <- substr(smk1$End, 1, 7)
    anal_jdate <- paste0(sprintf("%4d",year(anal_idate)),sprintf("%03d",yday(anal_idate)));anal_jdate
    
    rm(smk1_tmp)
    smk1_tmp <- raster::subset(smk1, start1 == anal_jdate & end1 == anal_jdate )
    head(smk1_tmp)
    
    
    # # select all sites for anal_date ---  
    library(dplyr)
    head(dat2)
    rm(dat2_idate)
    dat2_idate <- dat2 %>% dplyr::filter(DateObj == date_array[i]) %>% dplyr::select(site_date,longitude,latitude)
    
    
    if (length(smk1_tmp) >= 1) {
      # transform to polygon and polygon-sf --- 
      library(sf)
      smk1_tmp_sf <- sf::st_as_sf(smk1_tmp, coords=c("long","lat"), crs = st_crs(smk1))
      
      # transform obs point data to point-sf ---  
      pnts_sf <- sf::st_as_sf(dat2_idate, coords=c("longitude","latitude"), crs = st_crs(smk1))
      # intersect point to polygon, then extract HMS smk Density column
      dim(pnts_sf)
      rm(tmp0)
      tmp0 <- sf::st_intersects(pnts_sf, smk1_tmp_sf,sparse = FALSE)
      # row is site and column is against 9 smk polygons
      head(tmp0)
      
      # start putting SMK info for each monitor that intersects with polygons
      for (j in 1: dim(tmp0)[1]) {
        # check for not found intersect ---
        # (sum(tmp0, na.rm=TRUE) == 0 ) counts only TRUE ---
        if (sum(tmp0[j,], na.rm=TRUE) == 0 ) {
          # no intersect found
          dat2_idate$HMS_smk1[j] <- NA
          
        } else {
          # one or more intersects found
          tmp1 <- smk1_tmp_sf$Density[unlist(tmp0[j,])];tmp1
          # if an intersect found
          if (length(tmp1) == 1) { 
            dat2_idate$HMS_smk1[j] <- tmp1
          } else {
            # else bring the most frequent density category of smoke
            dat2_idate$HMS_smk1[j] <- names(which.max(table(tmp1))) 
            
          }
          print(paste("---Processing site ", j,"/",dim(tmp0)[1], dat2_idate[j,1], sep = " " ))
          
        } # if single or multi intersect are found
        
      } #j for sites
      
    } #if_exist subsetted date smk1
    
    if (i == 1 ) { dat2_smk2 <- dat2_idate } else { dat2_smk2 <- rbind(dat2_smk2,dat2_idate ) }
    
} # i for date   



head(dat2_smk1)  
head(dat2_smk2)

# found some duplicated entries in dat2_smk1, so take non-duplicated entries
rm(idx)
idx <- which(duplicated(dat2_smk1$site_date))

rm(smk1)
smk1 <- as.data.frame(dat2_smk1 %>% dplyr::select(site_date, longitude, latitude, HMS_smk0) %>% 
                        dplyr::group_by(site_date,longitude, latitude) %>% dplyr::distinct(HMS_smk0)) # dplyr::distinct() keep the unique row
dim(smk1)
# check for duplication
rm(idx)
idx <- which(duplicated(smk1$site_date))
idx

rm(smk2)
smk2 <- as.data.frame(dat2_smk2 %>% dplyr::select(site_date, longitude, latitude, HMS_smk1) %>% 
                        dplyr::group_by(site_date,longitude, latitude) %>% dplyr::distinct(HMS_smk1))
dim(dat2_smk2)
dim(smk1)
# check for duplication
rm(idx)
idx <- which(duplicated(smk2$site_date))
idx

# combined two dfs into 1 df ---
rm(dat2_smk)
dat2_smk <- dplyr::left_join(smk1,smk2, by = c("site_date","longitude", "latitude"))
head(dat2_smk)


# combine dat2_smk onto PM25 data ---
head(dat2)
rm(dat3)
dat3 <- dplyr::left_join(dat2,dat2_smk, by = c("site_date","longitude", "latitude"))
head(dat3)  
  
# CheckPoint-2
if (dim(dat3)[1] == dim(dat2)[1]) { writeLines("Good to go dat3 and dat2 have the same number of rows!") } else { writeLines("Stopped at CheckPoint-2"); break }

head(dat3)
glimpse(dat3)
# counting non-NA columns ---
dat3$tmp1 <- 0
rm(idx)
idx <- which(!is.na(dat3$HMS_smk0))
dat3$tmp1[idx] <- 1
  
dat3$tmp2 <- 0
rm(idx)
idx <- which(!is.na(dat3$HMS_smk1))
dat3$tmp2[idx] <- 1
  
dat3$HMS_smk01 <- NA
dat3$HMS_smk01 <- dat3$tmp1 +dat3$tmp2
unique(dat3$HMS_smk01)
# view check
tmp <- dat3[dat3$HMS_smk01 == 2,]
tmp
  
  
# drop off the connector ---
head(dat3)
dat3 <- subset(dat3, select = -c(site_date,tmp1,tmp2))
  
# converting smk Density to binary
dat3$HMS_binary <-0
rm(idx)
idx <- which(dat3$HMS_smk01 >0)
dat3$HMS_binary[idx] <- 1
unique(dat3$HMS_binary)
  
# write out data with proc_date
proc_year
proc_date
outdir <- "D:/My R workplace/ExceptionalEvent/airnow/airnow/Auto_Pull"

print("Writing out PM25_smk data")
outfile_DA_PM25 <- paste(outdir,"/airnow_PM25_24hr_smk_",proc_year,"_",proc_date,".csv",sep="");outfile_DA_PM25
write.table (dat3,outfile_DA_PM25,row.names=F,col.names=T,sep=",", quote=FALSE) 

#^^^-----------------------------------------------------

# END OF the PM25_HMS_smoke_timeseries.R code. NTsengel
