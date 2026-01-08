#' ---------------------------------------------------------
#' Script to load kelp forest SBC LTER data
#' and create dataframes to use for SEM variable creation
#' @date 2026-1-7 last update
#' @author Billie Beckley
#'-----------------------------------------------------------

# Waves -------------------------------------------------------

# Package ID: knb-lter-sbc.35.11 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Daily averages of modeled significant wave height (Hs) and peak wave period (Tp) in the Santa Barbara Coastal area from the Coastal Data Information Program - Monitoring and Prediction System (CDIP MOP).
# Data set creator:    - Coastal Data Information Program 
# Data set creator:  Tom W Bell -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/35/11/9d33fd69b20b41284431f197bce9e250" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "site",     
                 "date_GMT",     
                 "Mean_Hs_m",     
                 "Max_Hs_m",     
                 "Mean_Tp_m",     
                 "latitude",     
                 "longitude"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$site)!="factor") dt1$site<- as.factor(dt1$site)                                   
# attempting to convert dt1$date_GMT dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1date_GMT<-as.Date(dt1$date_GMT,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1date_GMT) == length(tmp1date_GMT[!is.na(tmp1date_GMT)])){dt1$date_GMT <- tmp1date_GMT } else {print("Date conversion failed for dt1$date_GMT. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1date_GMT) 
if (class(dt1$Mean_Hs_m)=="factor") dt1$Mean_Hs_m <-as.numeric(levels(dt1$Mean_Hs_m))[as.integer(dt1$Mean_Hs_m) ]               
if (class(dt1$Mean_Hs_m)=="character") dt1$Mean_Hs_m <-as.numeric(dt1$Mean_Hs_m)
if (class(dt1$Max_Hs_m)=="factor") dt1$Max_Hs_m <-as.numeric(levels(dt1$Max_Hs_m))[as.integer(dt1$Max_Hs_m) ]               
if (class(dt1$Max_Hs_m)=="character") dt1$Max_Hs_m <-as.numeric(dt1$Max_Hs_m)
if (class(dt1$Mean_Tp_m)=="factor") dt1$Mean_Tp_m <-as.numeric(levels(dt1$Mean_Tp_m))[as.integer(dt1$Mean_Tp_m) ]               
if (class(dt1$Mean_Tp_m)=="character") dt1$Mean_Tp_m <-as.numeric(dt1$Mean_Tp_m)
if (class(dt1$latitude)=="factor") dt1$latitude <-as.numeric(levels(dt1$latitude))[as.integer(dt1$latitude) ]               
if (class(dt1$latitude)=="character") dt1$latitude <-as.numeric(dt1$latitude)
if (class(dt1$longitude)=="factor") dt1$longitude <-as.numeric(levels(dt1$longitude))[as.integer(dt1$longitude) ]               
if (class(dt1$longitude)=="character") dt1$longitude <-as.numeric(dt1$longitude)

# Convert Missing Values to NA for non-dates

dt1$Mean_Hs_m <- ifelse((trimws(as.character(dt1$Mean_Hs_m))==trimws("-99999")),NA,dt1$Mean_Hs_m)               
suppressWarnings(dt1$Mean_Hs_m <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$Mean_Hs_m))==as.character(as.numeric("-99999"))),NA,dt1$Mean_Hs_m))
dt1$Max_Hs_m <- ifelse((trimws(as.character(dt1$Max_Hs_m))==trimws("-99999")),NA,dt1$Max_Hs_m)               
suppressWarnings(dt1$Max_Hs_m <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$Max_Hs_m))==as.character(as.numeric("-99999"))),NA,dt1$Max_Hs_m))
dt1$Mean_Tp_m <- ifelse((trimws(as.character(dt1$Mean_Tp_m))==trimws("-99999")),NA,dt1$Mean_Tp_m)               
suppressWarnings(dt1$Mean_Tp_m <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$Mean_Tp_m))==as.character(as.numeric("-99999"))),NA,dt1$Mean_Tp_m))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(site)
summary(date_GMT)
summary(Mean_Hs_m)
summary(Max_Hs_m)
summary(Mean_Tp_m)
summary(latitude)
summary(longitude) 
# Get more details on character variables

summary(as.factor(dt1$site))
detach(dt1)               


# Tidy
waves_daily_dat2 <- dt1; rm(dt1)



# Substrate/ Sand --------------------------------------------------------
# Package ID: knb-lter-sbc.138.3 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Kelp Forest Community Dynamics: Cover of bottom substrate and sand depth.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Data set creator:  Robert J Miller -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/138/3/82d1b4ba2b2c1b5438ae1279e19bf68b" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "QUAD",     
                 "SIDE",     
                 "SUBSTRATE_TYPE",     
                 "COMMON_NAME",     
                 "PERCENT_COVER"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$QUAD)!="factor") dt1$QUAD<- as.factor(dt1$QUAD)
if (class(dt1$SIDE)!="factor") dt1$SIDE<- as.factor(dt1$SIDE)
if (class(dt1$SUBSTRATE_TYPE)!="factor") dt1$SUBSTRATE_TYPE<- as.factor(dt1$SUBSTRATE_TYPE)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)

# Convert Missing Values to NA for non-dates

dt1$SITE <- as.factor(ifelse((trimws(as.character(dt1$SITE))==trimws("-99999")),NA,as.character(dt1$SITE)))
dt1$TRANSECT <- as.factor(ifelse((trimws(as.character(dt1$TRANSECT))==trimws("-99999")),NA,as.character(dt1$TRANSECT)))
dt1$QUAD <- as.factor(ifelse((trimws(as.character(dt1$QUAD))==trimws("-99999")),NA,as.character(dt1$QUAD)))
dt1$SIDE <- as.factor(ifelse((trimws(as.character(dt1$SIDE))==trimws("-99999")),NA,as.character(dt1$SIDE)))
dt1$SUBSTRATE_TYPE <- as.factor(ifelse((trimws(as.character(dt1$SUBSTRATE_TYPE))==trimws("-99999")),NA,as.character(dt1$SUBSTRATE_TYPE)))
dt1$COMMON_NAME <- as.factor(ifelse((trimws(as.character(dt1$COMMON_NAME))==trimws("-99999")),NA,as.character(dt1$COMMON_NAME)))
dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(YEAR)
summary(MONTH)
summary(DATE)
summary(SITE)
summary(TRANSECT)
summary(QUAD)
summary(SIDE)
summary(SUBSTRATE_TYPE)
summary(COMMON_NAME)
summary(PERCENT_COVER) 
# Get more details on character variables

summary(as.factor(dt1$SITE)) 
summary(as.factor(dt1$TRANSECT)) 
summary(as.factor(dt1$QUAD)) 
summary(as.factor(dt1$SIDE)) 
summary(as.factor(dt1$SUBSTRATE_TYPE)) 
summary(as.factor(dt1$COMMON_NAME))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/138/3/a46a2e69dbf70a0e75f669a4bacf18ca" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "DISTANCE",     
                 "SIDE",     
                 "SAND_DEPTH"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt2$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2DATE<-as.Date(dt2$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2DATE) == length(tmp2DATE[!is.na(tmp2DATE)])){dt2$DATE <- tmp2DATE } else {print("Date conversion failed for dt2$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2DATE) 
if (class(dt2$SITE)!="factor") dt2$SITE<- as.factor(dt2$SITE)
if (class(dt2$TRANSECT)!="factor") dt2$TRANSECT<- as.factor(dt2$TRANSECT)
if (class(dt2$DISTANCE)=="factor") dt2$DISTANCE <-as.numeric(levels(dt2$DISTANCE))[as.integer(dt2$DISTANCE) ]               
if (class(dt2$DISTANCE)=="character") dt2$DISTANCE <-as.numeric(dt2$DISTANCE)
if (class(dt2$SIDE)!="factor") dt2$SIDE<- as.factor(dt2$SIDE)
if (class(dt2$SAND_DEPTH)=="factor") dt2$SAND_DEPTH <-as.numeric(levels(dt2$SAND_DEPTH))[as.integer(dt2$SAND_DEPTH) ]               
if (class(dt2$SAND_DEPTH)=="character") dt2$SAND_DEPTH <-as.numeric(dt2$SAND_DEPTH)

# Convert Missing Values to NA for non-dates

dt2$SITE <- as.factor(ifelse((trimws(as.character(dt2$SITE))==trimws("-99999")),NA,as.character(dt2$SITE)))
dt2$TRANSECT <- as.factor(ifelse((trimws(as.character(dt2$TRANSECT))==trimws("-99999")),NA,as.character(dt2$TRANSECT)))
dt2$DISTANCE <- ifelse((trimws(as.character(dt2$DISTANCE))==trimws("-99999")),NA,dt2$DISTANCE)               
suppressWarnings(dt2$DISTANCE <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$DISTANCE))==as.character(as.numeric("-99999"))),NA,dt2$DISTANCE))
dt2$SIDE <- as.factor(ifelse((trimws(as.character(dt2$SIDE))==trimws("-99999")),NA,as.character(dt2$SIDE)))
dt2$SAND_DEPTH <- ifelse((trimws(as.character(dt2$SAND_DEPTH))==trimws("-99999")),NA,dt2$SAND_DEPTH)               
suppressWarnings(dt2$SAND_DEPTH <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SAND_DEPTH))==as.character(as.numeric("-99999"))),NA,dt2$SAND_DEPTH))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(YEAR)
summary(MONTH)
summary(DATE)
summary(SITE)
summary(TRANSECT)
summary(DISTANCE)
summary(SIDE)
summary(SAND_DEPTH) 
# Get more details on character variables

summary(as.factor(dt2$SITE)) 
summary(as.factor(dt2$TRANSECT)) 
summary(as.factor(dt2$SIDE))
detach(dt2)               


# Tidy
substrate.cover_by.date_dat2 <- dt1; rm(dt1)
sand.depth_by.date_dat2 <- dt2; rm(dt2)



# Urchins ------------------------------------------------------

# Package ID: knb-lter-sbc.50.13 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Annual time series of biomass for kelp forest species, ongoing since 2000.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Data set creator:  Robert J Miller -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/50/13/24d18d9ebe4f6e8b94e222840096963c" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "VIS",     
                 "SP_CODE",     
                 "PERCENT_COVER",     
                 "DENSITY",     
                 "WM_GM2",     
                 "DRY_GM2",     
                 "SFDM",     
                 "AFDM",     
                 "SCIENTIFIC_NAME",     
                 "COMMON_NAME",     
                 "TAXON_KINGDOM",     
                 "TAXON_PHYLUM",     
                 "TAXON_CLASS",     
                 "TAXON_ORDER",     
                 "TAXON_FAMILY",     
                 "TAXON_GENUS",     
                 "GROUP",     
                 "MOBILITY",     
                 "GROWTH_MORPH",     
                 "COARSE_GROUPING"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt1$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE<-as.Date(dt1$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE) == length(tmp1DATE[!is.na(tmp1DATE)])){dt1$DATE <- tmp1DATE } else {print("Date conversion failed for dt1$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE) 
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$TRANSECT)!="factor") dt1$TRANSECT<- as.factor(dt1$TRANSECT)
if (class(dt1$VIS)=="factor") dt1$VIS <-as.numeric(levels(dt1$VIS))[as.integer(dt1$VIS) ]               
if (class(dt1$VIS)=="character") dt1$VIS <-as.numeric(dt1$VIS)
if (class(dt1$SP_CODE)!="factor") dt1$SP_CODE<- as.factor(dt1$SP_CODE)
if (class(dt1$PERCENT_COVER)=="factor") dt1$PERCENT_COVER <-as.numeric(levels(dt1$PERCENT_COVER))[as.integer(dt1$PERCENT_COVER) ]               
if (class(dt1$PERCENT_COVER)=="character") dt1$PERCENT_COVER <-as.numeric(dt1$PERCENT_COVER)
if (class(dt1$DENSITY)=="factor") dt1$DENSITY <-as.numeric(levels(dt1$DENSITY))[as.integer(dt1$DENSITY) ]               
if (class(dt1$DENSITY)=="character") dt1$DENSITY <-as.numeric(dt1$DENSITY)
if (class(dt1$WM_GM2)=="factor") dt1$WM_GM2 <-as.numeric(levels(dt1$WM_GM2))[as.integer(dt1$WM_GM2) ]               
if (class(dt1$WM_GM2)=="character") dt1$WM_GM2 <-as.numeric(dt1$WM_GM2)
if (class(dt1$DRY_GM2)=="factor") dt1$DRY_GM2 <-as.numeric(levels(dt1$DRY_GM2))[as.integer(dt1$DRY_GM2) ]               
if (class(dt1$DRY_GM2)=="character") dt1$DRY_GM2 <-as.numeric(dt1$DRY_GM2)
if (class(dt1$SFDM)=="factor") dt1$SFDM <-as.numeric(levels(dt1$SFDM))[as.integer(dt1$SFDM) ]               
if (class(dt1$SFDM)=="character") dt1$SFDM <-as.numeric(dt1$SFDM)
if (class(dt1$AFDM)=="factor") dt1$AFDM <-as.numeric(levels(dt1$AFDM))[as.integer(dt1$AFDM) ]               
if (class(dt1$AFDM)=="character") dt1$AFDM <-as.numeric(dt1$AFDM)
if (class(dt1$SCIENTIFIC_NAME)!="factor") dt1$SCIENTIFIC_NAME<- as.factor(dt1$SCIENTIFIC_NAME)
if (class(dt1$COMMON_NAME)!="factor") dt1$COMMON_NAME<- as.factor(dt1$COMMON_NAME)
if (class(dt1$TAXON_KINGDOM)!="factor") dt1$TAXON_KINGDOM<- as.factor(dt1$TAXON_KINGDOM)
if (class(dt1$TAXON_PHYLUM)!="factor") dt1$TAXON_PHYLUM<- as.factor(dt1$TAXON_PHYLUM)
if (class(dt1$TAXON_CLASS)!="factor") dt1$TAXON_CLASS<- as.factor(dt1$TAXON_CLASS)
if (class(dt1$TAXON_ORDER)!="factor") dt1$TAXON_ORDER<- as.factor(dt1$TAXON_ORDER)
if (class(dt1$TAXON_FAMILY)!="factor") dt1$TAXON_FAMILY<- as.factor(dt1$TAXON_FAMILY)
if (class(dt1$TAXON_GENUS)!="factor") dt1$TAXON_GENUS<- as.factor(dt1$TAXON_GENUS)
if (class(dt1$GROUP)!="factor") dt1$GROUP<- as.factor(dt1$GROUP)
if (class(dt1$MOBILITY)!="factor") dt1$MOBILITY<- as.factor(dt1$MOBILITY)
if (class(dt1$GROWTH_MORPH)!="factor") dt1$GROWTH_MORPH<- as.factor(dt1$GROWTH_MORPH)
if (class(dt1$COARSE_GROUPING)!="factor") dt1$COARSE_GROUPING<- as.factor(dt1$COARSE_GROUPING)

# Convert Missing Values to NA for non-dates

dt1$VIS <- ifelse((trimws(as.character(dt1$VIS))==trimws("-99999")),NA,dt1$VIS)               
suppressWarnings(dt1$VIS <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$VIS))==as.character(as.numeric("-99999"))),NA,dt1$VIS))
dt1$PERCENT_COVER <- ifelse((trimws(as.character(dt1$PERCENT_COVER))==trimws("-99999")),NA,dt1$PERCENT_COVER)               
suppressWarnings(dt1$PERCENT_COVER <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$PERCENT_COVER))==as.character(as.numeric("-99999"))),NA,dt1$PERCENT_COVER))
dt1$DENSITY <- ifelse((trimws(as.character(dt1$DENSITY))==trimws("-99999")),NA,dt1$DENSITY)               
suppressWarnings(dt1$DENSITY <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DENSITY))==as.character(as.numeric("-99999"))),NA,dt1$DENSITY))
dt1$WM_GM2 <- ifelse((trimws(as.character(dt1$WM_GM2))==trimws("-99999")),NA,dt1$WM_GM2)               
suppressWarnings(dt1$WM_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$WM_GM2))==as.character(as.numeric("-99999"))),NA,dt1$WM_GM2))
dt1$DRY_GM2 <- ifelse((trimws(as.character(dt1$DRY_GM2))==trimws("-99999")),NA,dt1$DRY_GM2)               
suppressWarnings(dt1$DRY_GM2 <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$DRY_GM2))==as.character(as.numeric("-99999"))),NA,dt1$DRY_GM2))
dt1$SFDM <- ifelse((trimws(as.character(dt1$SFDM))==trimws("-99999")),NA,dt1$SFDM)               
suppressWarnings(dt1$SFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SFDM))==as.character(as.numeric("-99999"))),NA,dt1$SFDM))
dt1$AFDM <- ifelse((trimws(as.character(dt1$AFDM))==trimws("-99999")),NA,dt1$AFDM)               
suppressWarnings(dt1$AFDM <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$AFDM))==as.character(as.numeric("-99999"))),NA,dt1$AFDM))
dt1$SCIENTIFIC_NAME <- as.factor(ifelse((trimws(as.character(dt1$SCIENTIFIC_NAME))==trimws("-99999")),NA,as.character(dt1$SCIENTIFIC_NAME)))
dt1$TAXON_PHYLUM <- as.factor(ifelse((trimws(as.character(dt1$TAXON_PHYLUM))==trimws("-99999")),NA,as.character(dt1$TAXON_PHYLUM)))
dt1$TAXON_CLASS <- as.factor(ifelse((trimws(as.character(dt1$TAXON_CLASS))==trimws("-99999")),NA,as.character(dt1$TAXON_CLASS)))
dt1$TAXON_ORDER <- as.factor(ifelse((trimws(as.character(dt1$TAXON_ORDER))==trimws("-99999")),NA,as.character(dt1$TAXON_ORDER)))
dt1$TAXON_FAMILY <- as.factor(ifelse((trimws(as.character(dt1$TAXON_FAMILY))==trimws("-99999")),NA,as.character(dt1$TAXON_FAMILY)))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(YEAR)
summary(MONTH)
summary(DATE)
summary(SITE)
summary(TRANSECT)
summary(VIS)
summary(SP_CODE)
summary(PERCENT_COVER)
summary(DENSITY)
summary(WM_GM2)
summary(DRY_GM2)
summary(SFDM)
summary(AFDM)
summary(SCIENTIFIC_NAME)
summary(COMMON_NAME)
summary(TAXON_KINGDOM)
summary(TAXON_PHYLUM)
summary(TAXON_CLASS)
summary(TAXON_ORDER)
summary(TAXON_FAMILY)
summary(TAXON_GENUS)
summary(GROUP)
summary(MOBILITY)
summary(GROWTH_MORPH)
summary(COARSE_GROUPING) 
# Get more details on character variables

summary(as.factor(dt1$SITE)) 
summary(as.factor(dt1$TRANSECT)) 
summary(as.factor(dt1$SP_CODE)) 
summary(as.factor(dt1$SCIENTIFIC_NAME)) 
summary(as.factor(dt1$COMMON_NAME)) 
summary(as.factor(dt1$TAXON_KINGDOM)) 
summary(as.factor(dt1$TAXON_PHYLUM)) 
summary(as.factor(dt1$TAXON_CLASS)) 
summary(as.factor(dt1$TAXON_ORDER)) 
summary(as.factor(dt1$TAXON_FAMILY)) 
summary(as.factor(dt1$TAXON_GENUS)) 
summary(as.factor(dt1$GROUP)) 
summary(as.factor(dt1$MOBILITY)) 
summary(as.factor(dt1$GROWTH_MORPH)) 
summary(as.factor(dt1$COARSE_GROUPING))
detach(dt1)               


# Tidy
algae.invert.fish.biomass_by.date_dat2 <- dt1; rm(dt1)



# NPP Kelp ----------------------------------------------------------------
# Package ID: knb-lter-sbc.112.7 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: REEF:  Net primary production, growth and standing crop of Macrocystis pyrifera in Southern California.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Andrew A Rassweiler -  
# Data set creator:  Shannon Harrer -  
# Data set creator:  Daniel C Reed -  
# Data set creator:  Clint J Nelson -  
# Data set creator:  Robert J Miller -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/112/7/6f24a9026ff356773b93d16390f998b1" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site",     
                 "Year",     
                 "Season",     
                 "NPP_dry",     
                 "NPP_carbon",     
                 "NPP_nitrogen",     
                 "Growth_rate_dry",     
                 "Growth_rate_carbon",     
                 "Growth_rate_nitrogen",     
                 "SE_NPP_dry",     
                 "SE_NPP_carbon",     
                 "SE_NPP_nitrogen",     
                 "SE_growth_rate_dry",     
                 "SE_growth_rate_carbon",     
                 "SE_growth_rate_nitrogen"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$Site)!="factor") dt1$Site<- as.factor(dt1$Site)
if (class(dt1$Season)!="factor") dt1$Season<- as.factor(dt1$Season)
if (class(dt1$NPP_dry)=="factor") dt1$NPP_dry <-as.numeric(levels(dt1$NPP_dry))[as.integer(dt1$NPP_dry) ]               
if (class(dt1$NPP_dry)=="character") dt1$NPP_dry <-as.numeric(dt1$NPP_dry)
if (class(dt1$NPP_carbon)=="factor") dt1$NPP_carbon <-as.numeric(levels(dt1$NPP_carbon))[as.integer(dt1$NPP_carbon) ]               
if (class(dt1$NPP_carbon)=="character") dt1$NPP_carbon <-as.numeric(dt1$NPP_carbon)
if (class(dt1$NPP_nitrogen)=="factor") dt1$NPP_nitrogen <-as.numeric(levels(dt1$NPP_nitrogen))[as.integer(dt1$NPP_nitrogen) ]               
if (class(dt1$NPP_nitrogen)=="character") dt1$NPP_nitrogen <-as.numeric(dt1$NPP_nitrogen)
if (class(dt1$Growth_rate_dry)=="factor") dt1$Growth_rate_dry <-as.numeric(levels(dt1$Growth_rate_dry))[as.integer(dt1$Growth_rate_dry) ]               
if (class(dt1$Growth_rate_dry)=="character") dt1$Growth_rate_dry <-as.numeric(dt1$Growth_rate_dry)
if (class(dt1$Growth_rate_carbon)=="factor") dt1$Growth_rate_carbon <-as.numeric(levels(dt1$Growth_rate_carbon))[as.integer(dt1$Growth_rate_carbon) ]               
if (class(dt1$Growth_rate_carbon)=="character") dt1$Growth_rate_carbon <-as.numeric(dt1$Growth_rate_carbon)
if (class(dt1$Growth_rate_nitrogen)=="factor") dt1$Growth_rate_nitrogen <-as.numeric(levels(dt1$Growth_rate_nitrogen))[as.integer(dt1$Growth_rate_nitrogen) ]               
if (class(dt1$Growth_rate_nitrogen)=="character") dt1$Growth_rate_nitrogen <-as.numeric(dt1$Growth_rate_nitrogen)
if (class(dt1$SE_NPP_dry)=="factor") dt1$SE_NPP_dry <-as.numeric(levels(dt1$SE_NPP_dry))[as.integer(dt1$SE_NPP_dry) ]               
if (class(dt1$SE_NPP_dry)=="character") dt1$SE_NPP_dry <-as.numeric(dt1$SE_NPP_dry)
if (class(dt1$SE_NPP_carbon)=="factor") dt1$SE_NPP_carbon <-as.numeric(levels(dt1$SE_NPP_carbon))[as.integer(dt1$SE_NPP_carbon) ]               
if (class(dt1$SE_NPP_carbon)=="character") dt1$SE_NPP_carbon <-as.numeric(dt1$SE_NPP_carbon)
if (class(dt1$SE_NPP_nitrogen)=="factor") dt1$SE_NPP_nitrogen <-as.numeric(levels(dt1$SE_NPP_nitrogen))[as.integer(dt1$SE_NPP_nitrogen) ]               
if (class(dt1$SE_NPP_nitrogen)=="character") dt1$SE_NPP_nitrogen <-as.numeric(dt1$SE_NPP_nitrogen)
if (class(dt1$SE_growth_rate_dry)=="factor") dt1$SE_growth_rate_dry <-as.numeric(levels(dt1$SE_growth_rate_dry))[as.integer(dt1$SE_growth_rate_dry) ]               
if (class(dt1$SE_growth_rate_dry)=="character") dt1$SE_growth_rate_dry <-as.numeric(dt1$SE_growth_rate_dry)
if (class(dt1$SE_growth_rate_carbon)=="factor") dt1$SE_growth_rate_carbon <-as.numeric(levels(dt1$SE_growth_rate_carbon))[as.integer(dt1$SE_growth_rate_carbon) ]               
if (class(dt1$SE_growth_rate_carbon)=="character") dt1$SE_growth_rate_carbon <-as.numeric(dt1$SE_growth_rate_carbon)
if (class(dt1$SE_growth_rate_nitrogen)=="factor") dt1$SE_growth_rate_nitrogen <-as.numeric(levels(dt1$SE_growth_rate_nitrogen))[as.integer(dt1$SE_growth_rate_nitrogen) ]               
if (class(dt1$SE_growth_rate_nitrogen)=="character") dt1$SE_growth_rate_nitrogen <-as.numeric(dt1$SE_growth_rate_nitrogen)

# Convert Missing Values to NA for non-dates

dt1$NPP_dry <- ifelse((trimws(as.character(dt1$NPP_dry))==trimws("-99999")),NA,dt1$NPP_dry)               
suppressWarnings(dt1$NPP_dry <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$NPP_dry))==as.character(as.numeric("-99999"))),NA,dt1$NPP_dry))
dt1$NPP_carbon <- ifelse((trimws(as.character(dt1$NPP_carbon))==trimws("-99999")),NA,dt1$NPP_carbon)               
suppressWarnings(dt1$NPP_carbon <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$NPP_carbon))==as.character(as.numeric("-99999"))),NA,dt1$NPP_carbon))
dt1$NPP_nitrogen <- ifelse((trimws(as.character(dt1$NPP_nitrogen))==trimws("-99999")),NA,dt1$NPP_nitrogen)               
suppressWarnings(dt1$NPP_nitrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$NPP_nitrogen))==as.character(as.numeric("-99999"))),NA,dt1$NPP_nitrogen))
dt1$Growth_rate_dry <- ifelse((trimws(as.character(dt1$Growth_rate_dry))==trimws("-99999")),NA,dt1$Growth_rate_dry)               
suppressWarnings(dt1$Growth_rate_dry <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$Growth_rate_dry))==as.character(as.numeric("-99999"))),NA,dt1$Growth_rate_dry))
dt1$Growth_rate_carbon <- ifelse((trimws(as.character(dt1$Growth_rate_carbon))==trimws("-99999")),NA,dt1$Growth_rate_carbon)               
suppressWarnings(dt1$Growth_rate_carbon <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$Growth_rate_carbon))==as.character(as.numeric("-99999"))),NA,dt1$Growth_rate_carbon))
dt1$Growth_rate_nitrogen <- ifelse((trimws(as.character(dt1$Growth_rate_nitrogen))==trimws("-99999")),NA,dt1$Growth_rate_nitrogen)               
suppressWarnings(dt1$Growth_rate_nitrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$Growth_rate_nitrogen))==as.character(as.numeric("-99999"))),NA,dt1$Growth_rate_nitrogen))
dt1$SE_NPP_dry <- ifelse((trimws(as.character(dt1$SE_NPP_dry))==trimws("-99999")),NA,dt1$SE_NPP_dry)               
suppressWarnings(dt1$SE_NPP_dry <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SE_NPP_dry))==as.character(as.numeric("-99999"))),NA,dt1$SE_NPP_dry))
dt1$SE_NPP_carbon <- ifelse((trimws(as.character(dt1$SE_NPP_carbon))==trimws("-99999")),NA,dt1$SE_NPP_carbon)               
suppressWarnings(dt1$SE_NPP_carbon <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SE_NPP_carbon))==as.character(as.numeric("-99999"))),NA,dt1$SE_NPP_carbon))
dt1$SE_NPP_nitrogen <- ifelse((trimws(as.character(dt1$SE_NPP_nitrogen))==trimws("-99999")),NA,dt1$SE_NPP_nitrogen)               
suppressWarnings(dt1$SE_NPP_nitrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SE_NPP_nitrogen))==as.character(as.numeric("-99999"))),NA,dt1$SE_NPP_nitrogen))
dt1$SE_growth_rate_dry <- ifelse((trimws(as.character(dt1$SE_growth_rate_dry))==trimws("-99999")),NA,dt1$SE_growth_rate_dry)               
suppressWarnings(dt1$SE_growth_rate_dry <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SE_growth_rate_dry))==as.character(as.numeric("-99999"))),NA,dt1$SE_growth_rate_dry))
dt1$SE_growth_rate_carbon <- ifelse((trimws(as.character(dt1$SE_growth_rate_carbon))==trimws("-99999")),NA,dt1$SE_growth_rate_carbon)               
suppressWarnings(dt1$SE_growth_rate_carbon <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SE_growth_rate_carbon))==as.character(as.numeric("-99999"))),NA,dt1$SE_growth_rate_carbon))
dt1$SE_growth_rate_nitrogen <- ifelse((trimws(as.character(dt1$SE_growth_rate_nitrogen))==trimws("-99999")),NA,dt1$SE_growth_rate_nitrogen)               
suppressWarnings(dt1$SE_growth_rate_nitrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$SE_growth_rate_nitrogen))==as.character(as.numeric("-99999"))),NA,dt1$SE_growth_rate_nitrogen))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Site)
summary(Year)
summary(Season)
summary(NPP_dry)
summary(NPP_carbon)
summary(NPP_nitrogen)
summary(Growth_rate_dry)
summary(Growth_rate_carbon)
summary(Growth_rate_nitrogen)
summary(SE_NPP_dry)
summary(SE_NPP_carbon)
summary(SE_NPP_nitrogen)
summary(SE_growth_rate_dry)
summary(SE_growth_rate_carbon)
summary(SE_growth_rate_nitrogen) 
# Get more details on character variables

summary(as.factor(dt1$Site)) 
summary(as.factor(dt1$Season))
detach(dt1)               


inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/112/7/62685a4fb61873c1843433a3364fd08a" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site",     
                 "Date",     
                 "FSC_dry",     
                 "FSC_carbon",     
                 "FSC_nitrogen",     
                 "FSC_fraction_canopy",     
                 "Frond.density",     
                 "Plant_density",     
                 "Surface_Irradiance",     
                 "Bottom_Irradiance",     
                 "Subsurface_Irradiance",     
                 "Plant_loss_rate",     
                 "Frond_loss_rate",     
                 "Cut_frond_loss_rate",     
                 "Blade_loss_rate",     
                 "Dissolved_loss_rate",     
                 "Carbon_lost_as_plants",     
                 "Carbon_lost_as_fronds",     
                 "Carbon_lost_as_cut_fronds",     
                 "Carbon_lost_as_blades",     
                 "Carbon_lost_as_dissolved",     
                 "SE_FSC_dry",     
                 "SE_FSC_carbon",     
                 "SE_FSC_nitrogen",     
                 "SE_fraction_canopy",     
                 "SE_frond_density",     
                 "SE_plant_density",     
                 "SE_plant_loss_rate",     
                 "SE_frond_loss_rate",     
                 "SE_cut_frond_loss_rate",     
                 "SE_blade_loss_rate",     
                 "SE_dissolved_loss_rate",     
                 "SE_carbon_lost_as_plants",     
                 "SE_carbon_lost_as_fronds",     
                 "SE_carbon_lost_as_cut",     
                 "SE_carbon_lost_as_blades",     
                 "SE_carbon_lost_as_dissolved"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$Site)!="factor") dt2$Site<- as.factor(dt2$Site)                                   
# attempting to convert dt2$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2Date<-as.Date(dt2$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2Date) == length(tmp2Date[!is.na(tmp2Date)])){dt2$Date <- tmp2Date } else {print("Date conversion failed for dt2$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2Date) 
if (class(dt2$FSC_dry)=="factor") dt2$FSC_dry <-as.numeric(levels(dt2$FSC_dry))[as.integer(dt2$FSC_dry) ]               
if (class(dt2$FSC_dry)=="character") dt2$FSC_dry <-as.numeric(dt2$FSC_dry)
if (class(dt2$FSC_carbon)=="factor") dt2$FSC_carbon <-as.numeric(levels(dt2$FSC_carbon))[as.integer(dt2$FSC_carbon) ]               
if (class(dt2$FSC_carbon)=="character") dt2$FSC_carbon <-as.numeric(dt2$FSC_carbon)
if (class(dt2$FSC_nitrogen)=="factor") dt2$FSC_nitrogen <-as.numeric(levels(dt2$FSC_nitrogen))[as.integer(dt2$FSC_nitrogen) ]               
if (class(dt2$FSC_nitrogen)=="character") dt2$FSC_nitrogen <-as.numeric(dt2$FSC_nitrogen)
if (class(dt2$FSC_fraction_canopy)=="factor") dt2$FSC_fraction_canopy <-as.numeric(levels(dt2$FSC_fraction_canopy))[as.integer(dt2$FSC_fraction_canopy) ]               
if (class(dt2$FSC_fraction_canopy)=="character") dt2$FSC_fraction_canopy <-as.numeric(dt2$FSC_fraction_canopy)
if (class(dt2$Frond.density)=="factor") dt2$Frond.density <-as.numeric(levels(dt2$Frond.density))[as.integer(dt2$Frond.density) ]               
if (class(dt2$Frond.density)=="character") dt2$Frond.density <-as.numeric(dt2$Frond.density)
if (class(dt2$Plant_density)=="factor") dt2$Plant_density <-as.numeric(levels(dt2$Plant_density))[as.integer(dt2$Plant_density) ]               
if (class(dt2$Plant_density)=="character") dt2$Plant_density <-as.numeric(dt2$Plant_density)
if (class(dt2$Surface_Irradiance)=="factor") dt2$Surface_Irradiance <-as.numeric(levels(dt2$Surface_Irradiance))[as.integer(dt2$Surface_Irradiance) ]               
if (class(dt2$Surface_Irradiance)=="character") dt2$Surface_Irradiance <-as.numeric(dt2$Surface_Irradiance)
if (class(dt2$Bottom_Irradiance)=="factor") dt2$Bottom_Irradiance <-as.numeric(levels(dt2$Bottom_Irradiance))[as.integer(dt2$Bottom_Irradiance) ]               
if (class(dt2$Bottom_Irradiance)=="character") dt2$Bottom_Irradiance <-as.numeric(dt2$Bottom_Irradiance)
if (class(dt2$Subsurface_Irradiance)=="factor") dt2$Subsurface_Irradiance <-as.numeric(levels(dt2$Subsurface_Irradiance))[as.integer(dt2$Subsurface_Irradiance) ]               
if (class(dt2$Subsurface_Irradiance)=="character") dt2$Subsurface_Irradiance <-as.numeric(dt2$Subsurface_Irradiance)
if (class(dt2$Plant_loss_rate)=="factor") dt2$Plant_loss_rate <-as.numeric(levels(dt2$Plant_loss_rate))[as.integer(dt2$Plant_loss_rate) ]               
if (class(dt2$Plant_loss_rate)=="character") dt2$Plant_loss_rate <-as.numeric(dt2$Plant_loss_rate)
if (class(dt2$Frond_loss_rate)=="factor") dt2$Frond_loss_rate <-as.numeric(levels(dt2$Frond_loss_rate))[as.integer(dt2$Frond_loss_rate) ]               
if (class(dt2$Frond_loss_rate)=="character") dt2$Frond_loss_rate <-as.numeric(dt2$Frond_loss_rate)
if (class(dt2$Cut_frond_loss_rate)=="factor") dt2$Cut_frond_loss_rate <-as.numeric(levels(dt2$Cut_frond_loss_rate))[as.integer(dt2$Cut_frond_loss_rate) ]               
if (class(dt2$Cut_frond_loss_rate)=="character") dt2$Cut_frond_loss_rate <-as.numeric(dt2$Cut_frond_loss_rate)
if (class(dt2$Blade_loss_rate)=="factor") dt2$Blade_loss_rate <-as.numeric(levels(dt2$Blade_loss_rate))[as.integer(dt2$Blade_loss_rate) ]               
if (class(dt2$Blade_loss_rate)=="character") dt2$Blade_loss_rate <-as.numeric(dt2$Blade_loss_rate)
if (class(dt2$Dissolved_loss_rate)=="factor") dt2$Dissolved_loss_rate <-as.numeric(levels(dt2$Dissolved_loss_rate))[as.integer(dt2$Dissolved_loss_rate) ]               
if (class(dt2$Dissolved_loss_rate)=="character") dt2$Dissolved_loss_rate <-as.numeric(dt2$Dissolved_loss_rate)
if (class(dt2$Carbon_lost_as_plants)=="factor") dt2$Carbon_lost_as_plants <-as.numeric(levels(dt2$Carbon_lost_as_plants))[as.integer(dt2$Carbon_lost_as_plants) ]               
if (class(dt2$Carbon_lost_as_plants)=="character") dt2$Carbon_lost_as_plants <-as.numeric(dt2$Carbon_lost_as_plants)
if (class(dt2$Carbon_lost_as_fronds)=="factor") dt2$Carbon_lost_as_fronds <-as.numeric(levels(dt2$Carbon_lost_as_fronds))[as.integer(dt2$Carbon_lost_as_fronds) ]               
if (class(dt2$Carbon_lost_as_fronds)=="character") dt2$Carbon_lost_as_fronds <-as.numeric(dt2$Carbon_lost_as_fronds)
if (class(dt2$Carbon_lost_as_cut_fronds)=="factor") dt2$Carbon_lost_as_cut_fronds <-as.numeric(levels(dt2$Carbon_lost_as_cut_fronds))[as.integer(dt2$Carbon_lost_as_cut_fronds) ]               
if (class(dt2$Carbon_lost_as_cut_fronds)=="character") dt2$Carbon_lost_as_cut_fronds <-as.numeric(dt2$Carbon_lost_as_cut_fronds)
if (class(dt2$Carbon_lost_as_blades)=="factor") dt2$Carbon_lost_as_blades <-as.numeric(levels(dt2$Carbon_lost_as_blades))[as.integer(dt2$Carbon_lost_as_blades) ]               
if (class(dt2$Carbon_lost_as_blades)=="character") dt2$Carbon_lost_as_blades <-as.numeric(dt2$Carbon_lost_as_blades)
if (class(dt2$Carbon_lost_as_dissolved)=="factor") dt2$Carbon_lost_as_dissolved <-as.numeric(levels(dt2$Carbon_lost_as_dissolved))[as.integer(dt2$Carbon_lost_as_dissolved) ]               
if (class(dt2$Carbon_lost_as_dissolved)=="character") dt2$Carbon_lost_as_dissolved <-as.numeric(dt2$Carbon_lost_as_dissolved)
if (class(dt2$SE_FSC_dry)=="factor") dt2$SE_FSC_dry <-as.numeric(levels(dt2$SE_FSC_dry))[as.integer(dt2$SE_FSC_dry) ]               
if (class(dt2$SE_FSC_dry)=="character") dt2$SE_FSC_dry <-as.numeric(dt2$SE_FSC_dry)
if (class(dt2$SE_FSC_carbon)=="factor") dt2$SE_FSC_carbon <-as.numeric(levels(dt2$SE_FSC_carbon))[as.integer(dt2$SE_FSC_carbon) ]               
if (class(dt2$SE_FSC_carbon)=="character") dt2$SE_FSC_carbon <-as.numeric(dt2$SE_FSC_carbon)
if (class(dt2$SE_FSC_nitrogen)=="factor") dt2$SE_FSC_nitrogen <-as.numeric(levels(dt2$SE_FSC_nitrogen))[as.integer(dt2$SE_FSC_nitrogen) ]               
if (class(dt2$SE_FSC_nitrogen)=="character") dt2$SE_FSC_nitrogen <-as.numeric(dt2$SE_FSC_nitrogen)
if (class(dt2$SE_fraction_canopy)=="factor") dt2$SE_fraction_canopy <-as.numeric(levels(dt2$SE_fraction_canopy))[as.integer(dt2$SE_fraction_canopy) ]               
if (class(dt2$SE_fraction_canopy)=="character") dt2$SE_fraction_canopy <-as.numeric(dt2$SE_fraction_canopy)
if (class(dt2$SE_frond_density)=="factor") dt2$SE_frond_density <-as.numeric(levels(dt2$SE_frond_density))[as.integer(dt2$SE_frond_density) ]               
if (class(dt2$SE_frond_density)=="character") dt2$SE_frond_density <-as.numeric(dt2$SE_frond_density)
if (class(dt2$SE_plant_density)=="factor") dt2$SE_plant_density <-as.numeric(levels(dt2$SE_plant_density))[as.integer(dt2$SE_plant_density) ]               
if (class(dt2$SE_plant_density)=="character") dt2$SE_plant_density <-as.numeric(dt2$SE_plant_density)
if (class(dt2$SE_plant_loss_rate)=="factor") dt2$SE_plant_loss_rate <-as.numeric(levels(dt2$SE_plant_loss_rate))[as.integer(dt2$SE_plant_loss_rate) ]               
if (class(dt2$SE_plant_loss_rate)=="character") dt2$SE_plant_loss_rate <-as.numeric(dt2$SE_plant_loss_rate)
if (class(dt2$SE_frond_loss_rate)=="factor") dt2$SE_frond_loss_rate <-as.numeric(levels(dt2$SE_frond_loss_rate))[as.integer(dt2$SE_frond_loss_rate) ]               
if (class(dt2$SE_frond_loss_rate)=="character") dt2$SE_frond_loss_rate <-as.numeric(dt2$SE_frond_loss_rate)
if (class(dt2$SE_cut_frond_loss_rate)=="factor") dt2$SE_cut_frond_loss_rate <-as.numeric(levels(dt2$SE_cut_frond_loss_rate))[as.integer(dt2$SE_cut_frond_loss_rate) ]               
if (class(dt2$SE_cut_frond_loss_rate)=="character") dt2$SE_cut_frond_loss_rate <-as.numeric(dt2$SE_cut_frond_loss_rate)
if (class(dt2$SE_blade_loss_rate)=="factor") dt2$SE_blade_loss_rate <-as.numeric(levels(dt2$SE_blade_loss_rate))[as.integer(dt2$SE_blade_loss_rate) ]               
if (class(dt2$SE_blade_loss_rate)=="character") dt2$SE_blade_loss_rate <-as.numeric(dt2$SE_blade_loss_rate)
if (class(dt2$SE_dissolved_loss_rate)=="factor") dt2$SE_dissolved_loss_rate <-as.numeric(levels(dt2$SE_dissolved_loss_rate))[as.integer(dt2$SE_dissolved_loss_rate) ]               
if (class(dt2$SE_dissolved_loss_rate)=="character") dt2$SE_dissolved_loss_rate <-as.numeric(dt2$SE_dissolved_loss_rate)
if (class(dt2$SE_carbon_lost_as_plants)=="factor") dt2$SE_carbon_lost_as_plants <-as.numeric(levels(dt2$SE_carbon_lost_as_plants))[as.integer(dt2$SE_carbon_lost_as_plants) ]               
if (class(dt2$SE_carbon_lost_as_plants)=="character") dt2$SE_carbon_lost_as_plants <-as.numeric(dt2$SE_carbon_lost_as_plants)
if (class(dt2$SE_carbon_lost_as_fronds)=="factor") dt2$SE_carbon_lost_as_fronds <-as.numeric(levels(dt2$SE_carbon_lost_as_fronds))[as.integer(dt2$SE_carbon_lost_as_fronds) ]               
if (class(dt2$SE_carbon_lost_as_fronds)=="character") dt2$SE_carbon_lost_as_fronds <-as.numeric(dt2$SE_carbon_lost_as_fronds)
if (class(dt2$SE_carbon_lost_as_cut)=="factor") dt2$SE_carbon_lost_as_cut <-as.numeric(levels(dt2$SE_carbon_lost_as_cut))[as.integer(dt2$SE_carbon_lost_as_cut) ]               
if (class(dt2$SE_carbon_lost_as_cut)=="character") dt2$SE_carbon_lost_as_cut <-as.numeric(dt2$SE_carbon_lost_as_cut)
if (class(dt2$SE_carbon_lost_as_blades)=="factor") dt2$SE_carbon_lost_as_blades <-as.numeric(levels(dt2$SE_carbon_lost_as_blades))[as.integer(dt2$SE_carbon_lost_as_blades) ]               
if (class(dt2$SE_carbon_lost_as_blades)=="character") dt2$SE_carbon_lost_as_blades <-as.numeric(dt2$SE_carbon_lost_as_blades)
if (class(dt2$SE_carbon_lost_as_dissolved)=="factor") dt2$SE_carbon_lost_as_dissolved <-as.numeric(levels(dt2$SE_carbon_lost_as_dissolved))[as.integer(dt2$SE_carbon_lost_as_dissolved) ]               
if (class(dt2$SE_carbon_lost_as_dissolved)=="character") dt2$SE_carbon_lost_as_dissolved <-as.numeric(dt2$SE_carbon_lost_as_dissolved)

# Convert Missing Values to NA for non-dates

dt2$FSC_dry <- ifelse((trimws(as.character(dt2$FSC_dry))==trimws("-99999")),NA,dt2$FSC_dry)               
suppressWarnings(dt2$FSC_dry <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$FSC_dry))==as.character(as.numeric("-99999"))),NA,dt2$FSC_dry))
dt2$FSC_carbon <- ifelse((trimws(as.character(dt2$FSC_carbon))==trimws("-99999")),NA,dt2$FSC_carbon)               
suppressWarnings(dt2$FSC_carbon <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$FSC_carbon))==as.character(as.numeric("-99999"))),NA,dt2$FSC_carbon))
dt2$FSC_nitrogen <- ifelse((trimws(as.character(dt2$FSC_nitrogen))==trimws("-99999")),NA,dt2$FSC_nitrogen)               
suppressWarnings(dt2$FSC_nitrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$FSC_nitrogen))==as.character(as.numeric("-99999"))),NA,dt2$FSC_nitrogen))
dt2$FSC_fraction_canopy <- ifelse((trimws(as.character(dt2$FSC_fraction_canopy))==trimws("-99999")),NA,dt2$FSC_fraction_canopy)               
suppressWarnings(dt2$FSC_fraction_canopy <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$FSC_fraction_canopy))==as.character(as.numeric("-99999"))),NA,dt2$FSC_fraction_canopy))
dt2$Frond.density <- ifelse((trimws(as.character(dt2$Frond.density))==trimws("-99999")),NA,dt2$Frond.density)               
suppressWarnings(dt2$Frond.density <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Frond.density))==as.character(as.numeric("-99999"))),NA,dt2$Frond.density))
dt2$Plant_density <- ifelse((trimws(as.character(dt2$Plant_density))==trimws("-99999")),NA,dt2$Plant_density)               
suppressWarnings(dt2$Plant_density <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Plant_density))==as.character(as.numeric("-99999"))),NA,dt2$Plant_density))
dt2$Surface_Irradiance <- ifelse((trimws(as.character(dt2$Surface_Irradiance))==trimws("-99999")),NA,dt2$Surface_Irradiance)               
suppressWarnings(dt2$Surface_Irradiance <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Surface_Irradiance))==as.character(as.numeric("-99999"))),NA,dt2$Surface_Irradiance))
dt2$Bottom_Irradiance <- ifelse((trimws(as.character(dt2$Bottom_Irradiance))==trimws("-99999")),NA,dt2$Bottom_Irradiance)               
suppressWarnings(dt2$Bottom_Irradiance <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Bottom_Irradiance))==as.character(as.numeric("-99999"))),NA,dt2$Bottom_Irradiance))
dt2$Subsurface_Irradiance <- ifelse((trimws(as.character(dt2$Subsurface_Irradiance))==trimws("-99999")),NA,dt2$Subsurface_Irradiance)               
suppressWarnings(dt2$Subsurface_Irradiance <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Subsurface_Irradiance))==as.character(as.numeric("-99999"))),NA,dt2$Subsurface_Irradiance))
dt2$Plant_loss_rate <- ifelse((trimws(as.character(dt2$Plant_loss_rate))==trimws("-99999")),NA,dt2$Plant_loss_rate)               
suppressWarnings(dt2$Plant_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Plant_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$Plant_loss_rate))
dt2$Frond_loss_rate <- ifelse((trimws(as.character(dt2$Frond_loss_rate))==trimws("-99999")),NA,dt2$Frond_loss_rate)               
suppressWarnings(dt2$Frond_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Frond_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$Frond_loss_rate))
dt2$Cut_frond_loss_rate <- ifelse((trimws(as.character(dt2$Cut_frond_loss_rate))==trimws("-99999")),NA,dt2$Cut_frond_loss_rate)               
suppressWarnings(dt2$Cut_frond_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Cut_frond_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$Cut_frond_loss_rate))
dt2$Blade_loss_rate <- ifelse((trimws(as.character(dt2$Blade_loss_rate))==trimws("-99999")),NA,dt2$Blade_loss_rate)               
suppressWarnings(dt2$Blade_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Blade_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$Blade_loss_rate))
dt2$Dissolved_loss_rate <- ifelse((trimws(as.character(dt2$Dissolved_loss_rate))==trimws("-99999")),NA,dt2$Dissolved_loss_rate)               
suppressWarnings(dt2$Dissolved_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Dissolved_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$Dissolved_loss_rate))
dt2$Carbon_lost_as_plants <- ifelse((trimws(as.character(dt2$Carbon_lost_as_plants))==trimws("-99999")),NA,dt2$Carbon_lost_as_plants)               
suppressWarnings(dt2$Carbon_lost_as_plants <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Carbon_lost_as_plants))==as.character(as.numeric("-99999"))),NA,dt2$Carbon_lost_as_plants))
dt2$Carbon_lost_as_fronds <- ifelse((trimws(as.character(dt2$Carbon_lost_as_fronds))==trimws("-99999")),NA,dt2$Carbon_lost_as_fronds)               
suppressWarnings(dt2$Carbon_lost_as_fronds <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Carbon_lost_as_fronds))==as.character(as.numeric("-99999"))),NA,dt2$Carbon_lost_as_fronds))
dt2$Carbon_lost_as_cut_fronds <- ifelse((trimws(as.character(dt2$Carbon_lost_as_cut_fronds))==trimws("-99999")),NA,dt2$Carbon_lost_as_cut_fronds)               
suppressWarnings(dt2$Carbon_lost_as_cut_fronds <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Carbon_lost_as_cut_fronds))==as.character(as.numeric("-99999"))),NA,dt2$Carbon_lost_as_cut_fronds))
dt2$Carbon_lost_as_blades <- ifelse((trimws(as.character(dt2$Carbon_lost_as_blades))==trimws("-99999")),NA,dt2$Carbon_lost_as_blades)               
suppressWarnings(dt2$Carbon_lost_as_blades <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Carbon_lost_as_blades))==as.character(as.numeric("-99999"))),NA,dt2$Carbon_lost_as_blades))
dt2$Carbon_lost_as_dissolved <- ifelse((trimws(as.character(dt2$Carbon_lost_as_dissolved))==trimws("-99999")),NA,dt2$Carbon_lost_as_dissolved)               
suppressWarnings(dt2$Carbon_lost_as_dissolved <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$Carbon_lost_as_dissolved))==as.character(as.numeric("-99999"))),NA,dt2$Carbon_lost_as_dissolved))
dt2$SE_FSC_dry <- ifelse((trimws(as.character(dt2$SE_FSC_dry))==trimws("-99999")),NA,dt2$SE_FSC_dry)               
suppressWarnings(dt2$SE_FSC_dry <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_FSC_dry))==as.character(as.numeric("-99999"))),NA,dt2$SE_FSC_dry))
dt2$SE_FSC_carbon <- ifelse((trimws(as.character(dt2$SE_FSC_carbon))==trimws("-99999")),NA,dt2$SE_FSC_carbon)               
suppressWarnings(dt2$SE_FSC_carbon <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_FSC_carbon))==as.character(as.numeric("-99999"))),NA,dt2$SE_FSC_carbon))
dt2$SE_FSC_nitrogen <- ifelse((trimws(as.character(dt2$SE_FSC_nitrogen))==trimws("-99999")),NA,dt2$SE_FSC_nitrogen)               
suppressWarnings(dt2$SE_FSC_nitrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_FSC_nitrogen))==as.character(as.numeric("-99999"))),NA,dt2$SE_FSC_nitrogen))
dt2$SE_fraction_canopy <- ifelse((trimws(as.character(dt2$SE_fraction_canopy))==trimws("-99999")),NA,dt2$SE_fraction_canopy)               
suppressWarnings(dt2$SE_fraction_canopy <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_fraction_canopy))==as.character(as.numeric("-99999"))),NA,dt2$SE_fraction_canopy))
dt2$SE_frond_density <- ifelse((trimws(as.character(dt2$SE_frond_density))==trimws("-99999")),NA,dt2$SE_frond_density)               
suppressWarnings(dt2$SE_frond_density <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_frond_density))==as.character(as.numeric("-99999"))),NA,dt2$SE_frond_density))
dt2$SE_plant_density <- ifelse((trimws(as.character(dt2$SE_plant_density))==trimws("-99999")),NA,dt2$SE_plant_density)               
suppressWarnings(dt2$SE_plant_density <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_plant_density))==as.character(as.numeric("-99999"))),NA,dt2$SE_plant_density))
dt2$SE_plant_loss_rate <- ifelse((trimws(as.character(dt2$SE_plant_loss_rate))==trimws("-99999")),NA,dt2$SE_plant_loss_rate)               
suppressWarnings(dt2$SE_plant_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_plant_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$SE_plant_loss_rate))
dt2$SE_frond_loss_rate <- ifelse((trimws(as.character(dt2$SE_frond_loss_rate))==trimws("-99999")),NA,dt2$SE_frond_loss_rate)               
suppressWarnings(dt2$SE_frond_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_frond_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$SE_frond_loss_rate))
dt2$SE_cut_frond_loss_rate <- ifelse((trimws(as.character(dt2$SE_cut_frond_loss_rate))==trimws("-99999")),NA,dt2$SE_cut_frond_loss_rate)               
suppressWarnings(dt2$SE_cut_frond_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_cut_frond_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$SE_cut_frond_loss_rate))
dt2$SE_blade_loss_rate <- ifelse((trimws(as.character(dt2$SE_blade_loss_rate))==trimws("-99999")),NA,dt2$SE_blade_loss_rate)               
suppressWarnings(dt2$SE_blade_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_blade_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$SE_blade_loss_rate))
dt2$SE_dissolved_loss_rate <- ifelse((trimws(as.character(dt2$SE_dissolved_loss_rate))==trimws("-99999")),NA,dt2$SE_dissolved_loss_rate)               
suppressWarnings(dt2$SE_dissolved_loss_rate <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_dissolved_loss_rate))==as.character(as.numeric("-99999"))),NA,dt2$SE_dissolved_loss_rate))
dt2$SE_carbon_lost_as_plants <- ifelse((trimws(as.character(dt2$SE_carbon_lost_as_plants))==trimws("-99999")),NA,dt2$SE_carbon_lost_as_plants)               
suppressWarnings(dt2$SE_carbon_lost_as_plants <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_carbon_lost_as_plants))==as.character(as.numeric("-99999"))),NA,dt2$SE_carbon_lost_as_plants))
dt2$SE_carbon_lost_as_fronds <- ifelse((trimws(as.character(dt2$SE_carbon_lost_as_fronds))==trimws("-99999")),NA,dt2$SE_carbon_lost_as_fronds)               
suppressWarnings(dt2$SE_carbon_lost_as_fronds <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_carbon_lost_as_fronds))==as.character(as.numeric("-99999"))),NA,dt2$SE_carbon_lost_as_fronds))
dt2$SE_carbon_lost_as_cut <- ifelse((trimws(as.character(dt2$SE_carbon_lost_as_cut))==trimws("-99999")),NA,dt2$SE_carbon_lost_as_cut)               
suppressWarnings(dt2$SE_carbon_lost_as_cut <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_carbon_lost_as_cut))==as.character(as.numeric("-99999"))),NA,dt2$SE_carbon_lost_as_cut))
dt2$SE_carbon_lost_as_blades <- ifelse((trimws(as.character(dt2$SE_carbon_lost_as_blades))==trimws("-99999")),NA,dt2$SE_carbon_lost_as_blades)               
suppressWarnings(dt2$SE_carbon_lost_as_blades <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_carbon_lost_as_blades))==as.character(as.numeric("-99999"))),NA,dt2$SE_carbon_lost_as_blades))
dt2$SE_carbon_lost_as_dissolved <- ifelse((trimws(as.character(dt2$SE_carbon_lost_as_dissolved))==trimws("-99999")),NA,dt2$SE_carbon_lost_as_dissolved)               
suppressWarnings(dt2$SE_carbon_lost_as_dissolved <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt2$SE_carbon_lost_as_dissolved))==as.character(as.numeric("-99999"))),NA,dt2$SE_carbon_lost_as_dissolved))


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Site)
summary(Date)
summary(FSC_dry)
summary(FSC_carbon)
summary(FSC_nitrogen)
summary(FSC_fraction_canopy)
summary(Frond.density)
summary(Plant_density)
summary(Surface_Irradiance)
summary(Bottom_Irradiance)
summary(Subsurface_Irradiance)
summary(Plant_loss_rate)
summary(Frond_loss_rate)
summary(Cut_frond_loss_rate)
summary(Blade_loss_rate)
summary(Dissolved_loss_rate)
summary(Carbon_lost_as_plants)
summary(Carbon_lost_as_fronds)
summary(Carbon_lost_as_cut_fronds)
summary(Carbon_lost_as_blades)
summary(Carbon_lost_as_dissolved)
summary(SE_FSC_dry)
summary(SE_FSC_carbon)
summary(SE_FSC_nitrogen)
summary(SE_fraction_canopy)
summary(SE_frond_density)
summary(SE_plant_density)
summary(SE_plant_loss_rate)
summary(SE_frond_loss_rate)
summary(SE_cut_frond_loss_rate)
summary(SE_blade_loss_rate)
summary(SE_dissolved_loss_rate)
summary(SE_carbon_lost_as_plants)
summary(SE_carbon_lost_as_fronds)
summary(SE_carbon_lost_as_cut)
summary(SE_carbon_lost_as_blades)
summary(SE_carbon_lost_as_dissolved) 
# Get more details on character variables

summary(as.factor(dt2$Site))
detach(dt2)               


inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/112/7/d23ff8674913610988f2aa8d32ac2df1" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")


dt3 <-read.csv(infile3,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site",     
                 "Plant_ID",     
                 "Date",     
                 "Total_fronds",     
                 "New_fronds"    ), check.names=TRUE)

unlink(infile3)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt3$Site)!="factor") dt3$Site<- as.factor(dt3$Site)
if (class(dt3$Plant_ID)!="factor") dt3$Plant_ID<- as.factor(dt3$Plant_ID)                                   
# attempting to convert dt3$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp3Date<-as.Date(dt3$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3Date) == length(tmp3Date[!is.na(tmp3Date)])){dt3$Date <- tmp3Date } else {print("Date conversion failed for dt3$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3Date) 
if (class(dt3$Total_fronds)=="factor") dt3$Total_fronds <-as.numeric(levels(dt3$Total_fronds))[as.integer(dt3$Total_fronds) ]               
if (class(dt3$Total_fronds)=="character") dt3$Total_fronds <-as.numeric(dt3$Total_fronds)
if (class(dt3$New_fronds)=="factor") dt3$New_fronds <-as.numeric(levels(dt3$New_fronds))[as.integer(dt3$New_fronds) ]               
if (class(dt3$New_fronds)=="character") dt3$New_fronds <-as.numeric(dt3$New_fronds)

# Convert Missing Values to NA for non-dates

dt3$New_fronds <- ifelse((trimws(as.character(dt3$New_fronds))==trimws("-99999")),NA,dt3$New_fronds)               
suppressWarnings(dt3$New_fronds <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt3$New_fronds))==as.character(as.numeric("-99999"))),NA,dt3$New_fronds))


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Site)
summary(Plant_ID)
summary(Date)
summary(Total_fronds)
summary(New_fronds) 
# Get more details on character variables

summary(as.factor(dt3$Site)) 
summary(as.factor(dt3$Plant_ID))
detach(dt3)               


inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/112/7/471d15f0059405e8639425408aee4153" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")


dt4 <-read.csv(infile4,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Site",     
                 "Date",     
                 "Replicate",     
                 "Dry_to_wet_ratio",     
                 "Percent_carbon",     
                 "Percent_nitrogen",     
                 "Percent_hydrogen"    ), check.names=TRUE)

unlink(infile4)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt4$Site)!="factor") dt4$Site<- as.factor(dt4$Site)                                   
# attempting to convert dt4$Date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp4Date<-as.Date(dt4$Date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp4Date) == length(tmp4Date[!is.na(tmp4Date)])){dt4$Date <- tmp4Date } else {print("Date conversion failed for dt4$Date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp4Date) 
if (class(dt4$Replicate)!="factor") dt4$Replicate<- as.factor(dt4$Replicate)
if (class(dt4$Dry_to_wet_ratio)=="factor") dt4$Dry_to_wet_ratio <-as.numeric(levels(dt4$Dry_to_wet_ratio))[as.integer(dt4$Dry_to_wet_ratio) ]               
if (class(dt4$Dry_to_wet_ratio)=="character") dt4$Dry_to_wet_ratio <-as.numeric(dt4$Dry_to_wet_ratio)
if (class(dt4$Percent_carbon)=="factor") dt4$Percent_carbon <-as.numeric(levels(dt4$Percent_carbon))[as.integer(dt4$Percent_carbon) ]               
if (class(dt4$Percent_carbon)=="character") dt4$Percent_carbon <-as.numeric(dt4$Percent_carbon)
if (class(dt4$Percent_nitrogen)=="factor") dt4$Percent_nitrogen <-as.numeric(levels(dt4$Percent_nitrogen))[as.integer(dt4$Percent_nitrogen) ]               
if (class(dt4$Percent_nitrogen)=="character") dt4$Percent_nitrogen <-as.numeric(dt4$Percent_nitrogen)
if (class(dt4$Percent_hydrogen)=="factor") dt4$Percent_hydrogen <-as.numeric(levels(dt4$Percent_hydrogen))[as.integer(dt4$Percent_hydrogen) ]               
if (class(dt4$Percent_hydrogen)=="character") dt4$Percent_hydrogen <-as.numeric(dt4$Percent_hydrogen)

# Convert Missing Values to NA for non-dates

dt4$Dry_to_wet_ratio <- ifelse((trimws(as.character(dt4$Dry_to_wet_ratio))==trimws("-99999")),NA,dt4$Dry_to_wet_ratio)               
suppressWarnings(dt4$Dry_to_wet_ratio <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt4$Dry_to_wet_ratio))==as.character(as.numeric("-99999"))),NA,dt4$Dry_to_wet_ratio))
dt4$Percent_carbon <- ifelse((trimws(as.character(dt4$Percent_carbon))==trimws("-99999")),NA,dt4$Percent_carbon)               
suppressWarnings(dt4$Percent_carbon <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt4$Percent_carbon))==as.character(as.numeric("-99999"))),NA,dt4$Percent_carbon))
dt4$Percent_nitrogen <- ifelse((trimws(as.character(dt4$Percent_nitrogen))==trimws("-99999")),NA,dt4$Percent_nitrogen)               
suppressWarnings(dt4$Percent_nitrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt4$Percent_nitrogen))==as.character(as.numeric("-99999"))),NA,dt4$Percent_nitrogen))
dt4$Percent_hydrogen <- ifelse((trimws(as.character(dt4$Percent_hydrogen))==trimws("-99999")),NA,dt4$Percent_hydrogen)               
suppressWarnings(dt4$Percent_hydrogen <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt4$Percent_hydrogen))==as.character(as.numeric("-99999"))),NA,dt4$Percent_hydrogen))


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Site)
summary(Date)
summary(Replicate)
summary(Dry_to_wet_ratio)
summary(Percent_carbon)
summary(Percent_nitrogen)
summary(Percent_hydrogen) 
# Get more details on character variables

summary(as.factor(dt4$Site)) 
summary(as.factor(dt4$Replicate))
detach(dt4)               


inUrl5  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/112/7/6bc968141edf7b9e0a6bac923f5f78b2" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")


dt5 <-read.csv(infile5,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "Model",     
                 "Month",     
                 "Independent_variable",     
                 "Dependent_variable",     
                 "Slope",     
                 "Intercept",     
                 "pvalue",     
                 "rsquare",     
                 "N"    ), check.names=TRUE)

unlink(infile5)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt5$Model)!="factor") dt5$Model<- as.factor(dt5$Model)
if (class(dt5$Independent_variable)!="factor") dt5$Independent_variable<- as.factor(dt5$Independent_variable)
if (class(dt5$Dependent_variable)!="factor") dt5$Dependent_variable<- as.factor(dt5$Dependent_variable)
if (class(dt5$Slope)=="factor") dt5$Slope <-as.numeric(levels(dt5$Slope))[as.integer(dt5$Slope) ]               
if (class(dt5$Slope)=="character") dt5$Slope <-as.numeric(dt5$Slope)
if (class(dt5$Intercept)=="factor") dt5$Intercept <-as.numeric(levels(dt5$Intercept))[as.integer(dt5$Intercept) ]               
if (class(dt5$Intercept)=="character") dt5$Intercept <-as.numeric(dt5$Intercept)
if (class(dt5$pvalue)!="factor") dt5$pvalue<- as.factor(dt5$pvalue)
if (class(dt5$rsquare)=="factor") dt5$rsquare <-as.numeric(levels(dt5$rsquare))[as.integer(dt5$rsquare) ]               
if (class(dt5$rsquare)=="character") dt5$rsquare <-as.numeric(dt5$rsquare)
if (class(dt5$N)=="factor") dt5$N <-as.numeric(levels(dt5$N))[as.integer(dt5$N) ]               
if (class(dt5$N)=="character") dt5$N <-as.numeric(dt5$N)

# Convert Missing Values to NA for non-dates

dt5$Slope <- ifelse((trimws(as.character(dt5$Slope))==trimws("-99999")),NA,dt5$Slope)               
suppressWarnings(dt5$Slope <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt5$Slope))==as.character(as.numeric("-99999"))),NA,dt5$Slope))
dt5$Intercept <- ifelse((trimws(as.character(dt5$Intercept))==trimws("-99999")),NA,dt5$Intercept)               
suppressWarnings(dt5$Intercept <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt5$Intercept))==as.character(as.numeric("-99999"))),NA,dt5$Intercept))
dt5$rsquare <- ifelse((trimws(as.character(dt5$rsquare))==trimws("-99999")),NA,dt5$rsquare)               
suppressWarnings(dt5$rsquare <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt5$rsquare))==as.character(as.numeric("-99999"))),NA,dt5$rsquare))
dt5$N <- ifelse((trimws(as.character(dt5$N))==trimws("-99999")),NA,dt5$N)               
suppressWarnings(dt5$N <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt5$N))==as.character(as.numeric("-99999"))),NA,dt5$N))


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(Model)
summary(Month)
summary(Independent_variable)
summary(Dependent_variable)
summary(Slope)
summary(Intercept)
summary(pvalue)
summary(rsquare)
summary(N) 
# Get more details on character variables

summary(as.factor(dt5$Model)) 
summary(as.factor(dt5$Independent_variable)) 
summary(as.factor(dt5$Dependent_variable)) 
summary(as.factor(dt5$pvalue))
detach(dt5)               


inUrl6  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/112/7/f81190938c9835f6ba5f7b825cc9af35" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl"))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")


dt6 <-read.csv(infile6,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "YEAR",     
                 "MONTH",     
                 "DATE",     
                 "SITE",     
                 "TRANSECT",     
                 "SIDE",     
                 "DIST",     
                 "C_P_T",     
                 "F_1M",     
                 "F_SFC",     
                 "SFC_LONG",     
                 "MEAN_F",     
                 "DEPTH",     
                 "AREA"    ), check.names=TRUE)

unlink(infile6)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

# attempting to convert dt6$DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp6DATE<-as.Date(dt6$DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp6DATE) == length(tmp6DATE[!is.na(tmp6DATE)])){dt6$DATE <- tmp6DATE } else {print("Date conversion failed for dt6$DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp6DATE) 
if (class(dt6$SITE)!="factor") dt6$SITE<- as.factor(dt6$SITE)
if (class(dt6$TRANSECT)!="factor") dt6$TRANSECT<- as.factor(dt6$TRANSECT)
if (class(dt6$SIDE)!="factor") dt6$SIDE<- as.factor(dt6$SIDE)
if (class(dt6$DIST)=="factor") dt6$DIST <-as.numeric(levels(dt6$DIST))[as.integer(dt6$DIST) ]               
if (class(dt6$DIST)=="character") dt6$DIST <-as.numeric(dt6$DIST)
if (class(dt6$C_P_T)!="factor") dt6$C_P_T<- as.factor(dt6$C_P_T)
if (class(dt6$F_1M)=="factor") dt6$F_1M <-as.numeric(levels(dt6$F_1M))[as.integer(dt6$F_1M) ]               
if (class(dt6$F_1M)=="character") dt6$F_1M <-as.numeric(dt6$F_1M)
if (class(dt6$F_SFC)=="factor") dt6$F_SFC <-as.numeric(levels(dt6$F_SFC))[as.integer(dt6$F_SFC) ]               
if (class(dt6$F_SFC)=="character") dt6$F_SFC <-as.numeric(dt6$F_SFC)
if (class(dt6$SFC_LONG)=="factor") dt6$SFC_LONG <-as.numeric(levels(dt6$SFC_LONG))[as.integer(dt6$SFC_LONG) ]               
if (class(dt6$SFC_LONG)=="character") dt6$SFC_LONG <-as.numeric(dt6$SFC_LONG)
if (class(dt6$MEAN_F)=="factor") dt6$MEAN_F <-as.numeric(levels(dt6$MEAN_F))[as.integer(dt6$MEAN_F) ]               
if (class(dt6$MEAN_F)=="character") dt6$MEAN_F <-as.numeric(dt6$MEAN_F)
if (class(dt6$DEPTH)=="factor") dt6$DEPTH <-as.numeric(levels(dt6$DEPTH))[as.integer(dt6$DEPTH) ]               
if (class(dt6$DEPTH)=="character") dt6$DEPTH <-as.numeric(dt6$DEPTH)
if (class(dt6$AREA)=="factor") dt6$AREA <-as.numeric(levels(dt6$AREA))[as.integer(dt6$AREA) ]               
if (class(dt6$AREA)=="character") dt6$AREA <-as.numeric(dt6$AREA)

# Convert Missing Values to NA for non-dates

dt6$DIST <- ifelse((trimws(as.character(dt6$DIST))==trimws("-99999")),NA,dt6$DIST)               
suppressWarnings(dt6$DIST <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt6$DIST))==as.character(as.numeric("-99999"))),NA,dt6$DIST))
dt6$C_P_T <- as.factor(ifelse((trimws(as.character(dt6$C_P_T))==trimws("-99999")),NA,as.character(dt6$C_P_T)))
dt6$F_1M <- ifelse((trimws(as.character(dt6$F_1M))==trimws("-99999")),NA,dt6$F_1M)               
suppressWarnings(dt6$F_1M <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt6$F_1M))==as.character(as.numeric("-99999"))),NA,dt6$F_1M))
dt6$F_SFC <- ifelse((trimws(as.character(dt6$F_SFC))==trimws("-99999")),NA,dt6$F_SFC)               
suppressWarnings(dt6$F_SFC <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt6$F_SFC))==as.character(as.numeric("-99999"))),NA,dt6$F_SFC))
dt6$SFC_LONG <- ifelse((trimws(as.character(dt6$SFC_LONG))==trimws("-99999")),NA,dt6$SFC_LONG)               
suppressWarnings(dt6$SFC_LONG <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt6$SFC_LONG))==as.character(as.numeric("-99999"))),NA,dt6$SFC_LONG))
dt6$MEAN_F <- ifelse((trimws(as.character(dt6$MEAN_F))==trimws("-99999")),NA,dt6$MEAN_F)               
suppressWarnings(dt6$MEAN_F <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt6$MEAN_F))==as.character(as.numeric("-99999"))),NA,dt6$MEAN_F))
dt6$DEPTH <- ifelse((trimws(as.character(dt6$DEPTH))==trimws("-99999")),NA,dt6$DEPTH)               
suppressWarnings(dt6$DEPTH <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt6$DEPTH))==as.character(as.numeric("-99999"))),NA,dt6$DEPTH))
dt6$AREA <- ifelse((trimws(as.character(dt6$AREA))==trimws("-99999")),NA,dt6$AREA)               
suppressWarnings(dt6$AREA <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt6$AREA))==as.character(as.numeric("-99999"))),NA,dt6$AREA))


# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(YEAR)
summary(MONTH)
summary(DATE)
summary(SITE)
summary(TRANSECT)
summary(SIDE)
summary(DIST)
summary(C_P_T)
summary(F_1M)
summary(F_SFC)
summary(SFC_LONG)
summary(MEAN_F)
summary(DEPTH)
summary(AREA) 
# Get more details on character variables

summary(as.factor(dt6$SITE)) 
summary(as.factor(dt6$TRANSECT)) 
summary(as.factor(dt6$SIDE)) 
summary(as.factor(dt6$C_P_T))
detach(dt6)               

# Tidy up
kelp.npp_by.season_dat <- dt1; rm(dt1)
kelp.fsc_by.date_dat <- dt2; rm(dt2)
kelp.frond.census_by.date_dat <- dt3; rm(dt3)
kelp.cn_by.date_dat <- dt4; rm(dt4)
kelp.regression.parms_dat <- dt5; rm(dt5)
kelp.frond.dens_dat <- dt6; rm(dt6)



# Temperature --------------------------------------------------

# Package ID: knb-lter-sbc.13.29 Cataloging System:https://pasta.edirepository.org.
# Data set title: SBC LTER: Reef: Bottom Temperature: Continuous water temperature, ongoing since 2000.
# Data set creator:    - Santa Barbara Coastal LTER 
# Data set creator:  Daniel C Reed -  
# Data set creator:  Robert J Miller -  
# Contact:    - Information Manager, Santa Barbara Coastal LTER   - sbclter@msi.ucsb.edu
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-sbc/13/29/d707a45a2cd6eee1d016d99844d537da" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dt1 <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "SITE",     
                 "SERIAL",     
                 "DATE_LOCAL",     
                 "TIME_LOCAL",     
                 "TEMP_C"    ), check.names=TRUE)

unlink(infile1)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$SERIAL)!="factor") dt1$SERIAL<- as.factor(dt1$SERIAL)                                   
# attempting to convert dt1$DATE_LOCAL dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1DATE_LOCAL<-as.Date(dt1$DATE_LOCAL,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1DATE_LOCAL) == length(tmp1DATE_LOCAL[!is.na(tmp1DATE_LOCAL)])){dt1$DATE_LOCAL <- tmp1DATE_LOCAL } else {print("Date conversion failed for dt1$DATE_LOCAL. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1DATE_LOCAL) 
if (class(dt1$TEMP_C)=="factor") dt1$TEMP_C <-as.numeric(levels(dt1$TEMP_C))[as.integer(dt1$TEMP_C) ]               
if (class(dt1$TEMP_C)=="character") dt1$TEMP_C <-as.numeric(dt1$TEMP_C)

# Convert Missing Values to NA for non-dates

dt1$SITE <- as.factor(ifelse((trimws(as.character(dt1$SITE))==trimws("-99999")),NA,as.character(dt1$SITE)))
dt1$SERIAL <- as.factor(ifelse((trimws(as.character(dt1$SERIAL))==trimws("-99999")),NA,as.character(dt1$SERIAL)))
dt1$TEMP_C <- ifelse((trimws(as.character(dt1$TEMP_C))==trimws("-99999")),NA,dt1$TEMP_C)               
suppressWarnings(dt1$TEMP_C <- ifelse(!is.na(as.numeric("-99999")) & (trimws(as.character(dt1$TEMP_C))==as.character(as.numeric("-99999"))),NA,dt1$TEMP_C))


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(SITE)
summary(SERIAL)
summary(DATE_LOCAL)
summary(TIME_LOCAL)
summary(TEMP_C) 
# Get more details on character variables

summary(as.factor(dt1$SITE)) 
summary(as.factor(dt1$SERIAL))
detach(dt1)               


# Tidy
temp_by.10min_dat2 <- dt1; rm(dt1)





