#' ---------------------------------------------------------
#' Script to build derived NPP dataset for SEM
#' using SBC LTER data
#' @date 2026-1-7 last update
#' @author Billie Beckley
#'-----------------------------------------------------------

# load libraries ----------------------------------------------------------
library(tidyverse)
library(dplyr)
library(lubridate)
library(car)

# Annual NPP --------------------------------------------------------------

#copy NPP dataframe
NPPdata <- kelp.npp_by.season_dat

#kelp NPP summed for all seasons in the year (as defined by solstice and equinoxes) year begins in spring
#currently, NPP_dry is kg/m^2 per day. multiply current NPP value by number of days in each season 

# Calculate number of days per season
no.days.winter = (365 - 355) + (79 - 0)
no.days.spring = 172-79
no.days.summer = 265-172
no.days.autumn = 355-265

# Check
no.days.winter + no.days.spring + no.days.summer + no.days.autumn == 365

# no.days
no.days.winter #89
no.days.spring #93
no.days.summer #93
no.days.autumn #90


#multiply NPP_dry by number of days in each season

NPPdata$NPP_dry_per.season[NPPdata$Season == "1-Winter"] <-
  NPPdata$NPP_dry[NPPdata$Season == "1-Winter"] * no.days.winter

NPPdata$NPP_dry_per.season[NPPdata$Season == "2-Spring"] <- 
  NPPdata$NPP_dry[NPPdata$Season == "2-Spring"] * no.days.spring

NPPdata$NPP_dry_per.season[NPPdata$Season == "3-Summer"] <- 
  NPPdata$NPP_dry[NPPdata$Season == "3-Summer"] * no.days.summer

NPPdata$NPP_dry_per.season[NPPdata$Season == "4-Autumn"] <- 
  NPPdata$NPP_dry[NPPdata$Season == "4-Autumn"] * no.days.autumn

#create a kelp.year column
NPPdata$kelp.year <- NPPdata$Year

#move kelp.year next to year column
NPPdata <- NPPdata %>% dplyr::relocate(kelp.year, .before = "Season")

#change winter to be same kelp year
NPPdata$kelp.year[NPPdata$Season == "1-Winter"] <- NPPdata$kelp.year[NPPdata$Season == "1-Winter"] - 1


#create kelp.year (all seasons beginning in spring)
npp_biomass_variables <- NPPdata %>% dplyr::group_by(Site, kelp.year) %>% 
  dplyr::filter(kelp.year != 2002) %>% 
  dplyr::filter(kelp.year != 2020) %>% 
  dplyr::filter(kelp.year != 2024) %>% 
  dplyr::summarise(NPP_dry_per.year = sum(NPP_dry_per.season),
                   sd = sd(NPP_dry_per.season, na.rm = TRUE)) %>% 
  dplyr::arrange(Site) %>% 
  dplyr::ungroup()


# Initial Kelp Biomass -----------------------------------------------------------

#foliar standing crop for April

#make new df
kelp.biomass <- kelp.fsc_by.date_dat %>%  #copy dataframe
  dplyr::select(c(Site, Date, FSC_dry)) %>% #select relevant columns
  separate(Date, sep="-", into = c("Year", "Month", "Day")) #separate Year-Month-Day into separate columns

#make month numeric
kelp.biomass$Month <- as.numeric(kelp.biomass$Month)

#change AQUE 3/31/09 to AQUE april measurement
kelp.biomass$Month[kelp.biomass$Site == "AQUE" &
                     kelp.biomass$Year == 2009 &
                     kelp.biomass$Month == 03 &
                     kelp.biomass$Day == 31] <- kelp.biomass$Month[kelp.biomass$Site == "AQUE" &
                                                                     kelp.biomass$Year == 2009 &
                                                                     kelp.biomass$Month == 03 &
                                                                     kelp.biomass$Day == 31] +1


#make month numeric
kelp.biomass$Month <- as.character(kelp.biomass$Month)

APRIL_BIOMASS <- kelp.biomass %>% 
  dplyr::filter(Month == 04) %>% 
  dplyr::filter(Year != "2002") %>% #remove 2002
  dplyr::filter(Year != 2020) %>%
  dplyr::filter(Year != 2024) %>%
  dplyr::group_by(Year) %>% 
  tidyr::complete(Site) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Site)



#add to df
npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(APRIL_BIOMASS = APRIL_BIOMASS$FSC_dry)



# Growth Rate -------------------------------------------------------------

#copy df
growth.rate.df <- kelp.npp_by.season_dat


#create a kelp.year column
growth.rate.df$kelp.year <- growth.rate.df$Year

#move kelp.year next to year column
growth.rate.df <- growth.rate.df %>% dplyr::relocate(kelp.year, .before = "Season")

#change winter to be same kelp year
growth.rate.df$kelp.year[growth.rate.df$Season == "1-Winter"] <- growth.rate.df$kelp.year[growth.rate.df$Season == "1-Winter"] - 1

#mean annual growth rate 
growth.rate.df <- growth.rate.df %>%
  dplyr::select(Site, kelp.year, Year, Season, Growth_rate_dry) %>% 
  dplyr::group_by(Site, kelp.year) %>% 
  dplyr::summarise(KELP_GROWTH = mean(Growth_rate_dry),
                   sd = sd(Growth_rate_dry)) %>%
  dplyr::ungroup() %>% 
  dplyr::filter(kelp.year != "2001") %>% 
  dplyr::filter(kelp.year != "2002") %>% 
  dplyr::filter(kelp.year != "2020") %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(Site) %>% 
  dplyr::filter(kelp.year != "2024")


#add df to sem df
npp_biomass_variables <- npp_biomass_variables %>% 
  add_column(KELP_GROWTH = growth.rate.df$KELP_GROWTH)


# Annual Recruitment ------------------------------------------------------

#mean density of recruits (plants with < or = 2 fronds that are < or = 2m avg height) average of June, July and August in the current year

#copy df
kelp_frond.data <- kelp.frond.dens_dat %>% 
  separate(DATE, sep="-", into = c("Year", "Month", "Day")) #separate Year-Month-Day into separate columns

kelp_frond.data$Month <- as.numeric(kelp_frond.data$Month)

#calculate transect area
npp_transect_area <- kelp_frond.data %>%
  dplyr::select(Year, Month, SITE, TRANSECT, SIDE, F_1M, MEAN_F, AREA) %>% 
  unite(col='TSIDE', c('TRANSECT', 'SIDE'), sep="") %>% #rename each transect/side combo a unique name
  dplyr::group_by(SITE, Year, TSIDE) %>% 
  dplyr:: summarise(AREA_AVE = mean(AREA)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(SITE, Year) %>% 
  dplyr::summarise(AREA_TOTAL = sum(AREA_AVE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(SITE) %>% 
  dplyr::filter(Year != "2002") %>% 
  dplyr::filter(Year != "2020")  %>% 
  dplyr::filter(Year != "2024")  %>% 
  dplyr::arrange(SITE) %>% 
  dplyr::ungroup()


recruits <- kelp_frond.data %>%
  dplyr::select(Year, Month, SITE, F_1M, MEAN_F) %>% 
  drop_na() %>% 
  dplyr::filter(Month > 05 & Month < 09) %>% #select months june, july & august
  dplyr::filter(F_1M <= 2 & MEAN_F <= 2) %>% #select plants that are < or = 2 fronds that are < or = 2m height
  dplyr::group_by(SITE, Year, Month) %>% 
  dplyr:: summarise(n_recruits = length(F_1M)) %>% #number of plants that meet the conditions in each month (i.e. density)
  dplyr::ungroup() %>% 
  complete(SITE, Year, Month, fill = list(n_recruits = 0)) %>% 
  dplyr::group_by(SITE, Year) %>% 
  dplyr::summarise(KELP_RECRUITS_AVE = mean(n_recruits)) %>% #average density over all 3 months in each year
  dplyr::filter(Year != "2002") %>% 
  dplyr::filter(Year != "2020") %>% 
  dplyr::filter(Year != "2024") %>% 
  dplyr::ungroup() %>% 
  dplyr:: rename(kelp.year = Year)  %>% #rename YEAR to match npp sem df
  dplyr::ungroup() %>% 
  dplyr::arrange(SITE) %>% 
  dplyr::mutate(AREA_TOTAL = npp_transect_area$AREA_TOTAL) %>% 
  dplyr::mutate(RECRUITS_M2 = KELP_RECRUITS_AVE/AREA_TOTAL) %>% 
  dplyr::ungroup() 

#add df to sem df
npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(KELP_RECRUITS_M2 = recruits$RECRUITS_M2) %>% 
  dplyr::mutate(AREA_TOTAL = npp_transect_area$AREA_TOTAL)


# Grazing Capacity  -------------------------------------------------------

#average biomass of urchins (red+purple+white+large+small) on the NPP transect in the summer of the current year
#average biomass of urchins (red+purple+white+large+small) on the NPP transect in the summer of the prior year

#copy algae.invert.fish.biomass_by.date_dat dataframe
algae.invert.fish.biomass_data2 <- algae.invert.fish.biomass_by.date_dat2


#calculate variable 
urchin_biomass.df <- algae.invert.fish.biomass_data2%>% 
  dplyr::group_by(YEAR, SITE) %>% 
  dplyr::filter(c (SITE == "AQUE" & TRANSECT == "1" |
                     SITE == "MOHK" & TRANSECT == "1" |
                     SITE == "ABUR" & TRANSECT == "1")) %>% #NPP SITES
  dplyr::select(c(YEAR, SITE, TRANSECT, SP_CODE, WM_GM2, SCIENTIFIC_NAME)) %>% #select for all white, purple, red urchins
  dplyr::filter(c(SCIENTIFIC_NAME == "Strongylocentrotus purpuratus" |
                    SCIENTIFIC_NAME == "Mesocentrotus franciscanus")) %>% 
  dplyr::mutate(GRAZING_RATE = case_when(SCIENTIFIC_NAME == "Strongylocentrotus purpuratus" ~ WM_GM2 * 0.02,
                                         SCIENTIFIC_NAME == "Mesocentrotus franciscanus" ~ WM_GM2 * 0.0091)) %>% 
  dplyr::summarise(URCHIN_GRAZING = mean(GRAZING_RATE)) %>% #average urchin biomass current year
  dplyr::ungroup() %>% 
  dplyr::group_by(SITE) %>% 
  dplyr::mutate(URCHIN_GRAZING_PREV = lag(URCHIN_GRAZING)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(YEAR, SITE) %>% 
  dplyr::mutate(URCHIN_GRAZING_AVE = mean(c_across(c('URCHIN_GRAZING', 'URCHIN_GRAZING_PREV')), na.rm=TRUE)) %>% 
  dplyr::rename(kelp.year = YEAR) %>%  #change YEAR to kelp.year to match dataframe 
  dplyr::filter(kelp.year != "2001") %>% #remove 2001 data
  dplyr::filter(kelp.year != "2002") %>% #remove 2002 data
  dplyr::filter(kelp.year != "2020") %>% #remove 2020 data
  dplyr::filter(kelp.year != "2024") %>% #remove 2020 data
  dplyr::ungroup() %>% 
  dplyr::arrange(SITE)


#move urchin bmass columns to  npp_sem_variables df
npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(URCHIN_GRAZING_AVE = urchin_biomass.df$URCHIN_GRAZING_AVE)

# Loss --------------------------------------------------------------------

#make df
all_years_l <- read.csv("Derived Data/NPP_ALL_YEARS_seasonal_with_MC_stderr.csv")
#loss rate data for each kelp year

#kelp.year to numeric
all_years_l <- all_years_l  %>% 
  dplyr::select(YEAR, MONTH, date, SITE, l) %>% 
  dplyr::mutate(kelp.year = YEAR) 

all_years_l$kelp.year <- as.numeric( all_years_l$kelp.year)

#are missing values 0??
all_years_l_lagged <- all_years_l %>% 
  dplyr::filter(MONTH == 1 | MONTH == 2 | MONTH == 3 | MONTH == 4) %>% 
  dplyr::group_by(SITE, kelp.year) %>% 
  dplyr::summarise(ave_l = mean(l, na.rm = TRUE),
                   max_l = max(l, na.rm = TRUE)) %>% 
  mutate_at(.vars = vars(max_l), function(x) ifelse(is.infinite(x), NA, x)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(kelp.year != 2002) %>% 
  dplyr::filter(kelp.year != 2020) %>% 
  dplyr::filter(kelp.year != 2024) %>% 
  dplyr::arrange(SITE)

npp_biomass_variables <- npp_biomass_variables %>% dplyr::mutate(l_lagged = all_years_l_lagged$ave_l)


# Waves -------------------------------------------------------------------

new_waves <- read.csv("Billie/20 Years Data!/Kelp_frond_loss_with_maxwave_121724.csv")
#wave height for each sampling period 

#copy df
large_waves.df <- new_waves %>% 
  dplyr::filter(c (site == "AQUE"| site == "MOHK"| site == "ABUR")) %>% 
  separate(date, sep="-", into = c("YEAR", "MONTH", "DAY"))

large_waves.df$MONTH <- as.integer(large_waves.df$MONTH)
large_waves.df$YEAR <- as.integer(large_waves.df$YEAR)
large_waves.df$DAY <- as.integer(large_waves.df$DAY)

#this dataframe has site specific info
large_waves.df1 <- large_waves.df %>% 
  dplyr::select(c(site, YEAR, MONTH, DAY, Max_Hs_m)) %>% 
  dplyr::filter(MONTH >= 01 & MONTH < 04 | MONTH == "12") %>% #select winter months (12-3)
  dplyr::group_by(site, YEAR, MONTH, DAY) %>% 
  mutate(t = make_datetime(YEAR, MONTH, DAY)) 

#create a kelp.year column
large_waves.df1$kelp.year <- large_waves.df1$YEAR
large_waves.df1$kelp.year <- as.numeric(large_waves.df1$kelp.year)

#move kelp.year next to year column
large_waves.df1 <- large_waves.df1%>% 
  dplyr::relocate(kelp.year, .before = "MONTH")

#change winter to be same kelp year as month 12 which would make it previous kelp year
large_waves.df1$kelp.year[large_waves.df1$MONTH == "12"] <- large_waves.df1$kelp.year[large_waves.df1$MONTH == "12"] + 1


large_waves_max <- large_waves.df1 %>% 
  dplyr::group_by(site, kelp.year) %>% 
  dplyr::summarise(Ave_max_wave = mean(Max_Hs_m)) %>% 
  dplyr::filter(kelp.year != "2002") %>% #remove 2002
  dplyr::filter(kelp.year != "1999") %>%  #remove 2020
  dplyr::filter(kelp.year != "2000") %>% #remove 2002
  dplyr::filter(kelp.year != "2001") %>%  #remove 2002
  dplyr::filter(kelp.year != "2020") %>%   #remove 2002
  dplyr::filter(kelp.year != "2024") %>%   #remove 2002
  dplyr::arrange(site) %>% 
  dplyr::ungroup()  


npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(LARGE_WAVES_MAX = large_waves_max$Ave_max_wave)


# Nitrate Recruit Threshold ---------------------------------------


# STEP 1: Create nitrate estimates from temperature
tempdata <- temp_by.10min_dat2 %>%
  filter(SITE %in% c("AQUE", "MOHK", "ABUR")) %>%
  mutate(
    NITRATE = case_when(
      SITE == "AQUE" & TEMP_C <= 15.50 ~ -0.00628747 * (TEMP_C^4) + 0.32811 * (TEMP_C^3) - 5.7165 * (TEMP_C^2) + 33.833 * TEMP_C - 9.7322,
      SITE == "AQUE" & TEMP_C > 15.50  ~ 0.22,
      SITE %in% c("MOHK", "ABUR")      ~ (2.8 * 10^4) * exp(-(0.64) * TEMP_C)
    )
  ) %>%
  separate(DATE_LOCAL, into = c("YEAR", "MONTH", "DAY"), sep = "-", convert = TRUE) %>%
  mutate(DATE = as.Date(paste(YEAR, MONTH, DAY, sep = "-")))

# STEP 2: Summarize total days with temperature data per site/year
temp_day_counts <- tempdata %>%
  group_by(SITE, YEAR) %>%
  summarise(total_temp_days = n_distinct(DATE), .groups = "drop")

# STEP 3: Calculate daily nitrate means during Apr–Aug
nitrate_daily <- tempdata %>%
  filter(MONTH >= 4 & MONTH <= 8) %>%  # April–August window
  group_by(SITE, YEAR, DATE) %>%
  summarise(NITRATE_daily_MEAN = mean(NITRATE, na.rm = TRUE), .groups = "drop")

# STEP 4: Count days with nitrate < 1 for each site/year
nitrate_threshold_counts <- nitrate_daily %>%
  group_by(SITE, YEAR) %>%
  summarise(days_below_1 = sum(NITRATE_daily_MEAN < 1, na.rm = TRUE), .groups = "drop")

# STEP 5: Join with total days and set NA if total days < 357
nitrate_threshold_summary <- full_join(nitrate_threshold_counts, temp_day_counts, by = c("SITE", "YEAR")) %>%
  mutate(
    days_below_1 = ifelse(total_temp_days < 357, NA, days_below_1)
  ) %>%
  rename(kelp.year = YEAR) %>%
  dplyr::filter(kelp.year != "2002") %>%  #remove 2002
  dplyr::filter(kelp.year != "2020") %>%   #remove 2002
  dplyr::filter(kelp.year != "2024") %>%   #remove 2002
  arrange(SITE)

npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(NITRATE_THRESHOLD_RECRUIT = nitrate_threshold_summary$days_below_1)

# Sand  --------------------------------------------------------------------

#copy substrate.cover_by.date_dat dataframe
substrate.df2 <- substrate.cover_by.date_dat2

Sand.df <- substrate.df2 %>% group_by(YEAR, SITE) %>% 
  dplyr::filter(c (SITE == "AQUE" & TRANSECT == "1" |
                     SITE == "MOHK" & TRANSECT == "1" |
                     SITE == "ABUR" & TRANSECT == "1")) %>% #NPP SITES
  filter(SUBSTRATE_TYPE == "S") %>% 
  dplyr::group_by(SITE, YEAR) %>% 
  dplyr::summarise(SAND_COVER = mean(PERCENT_COVER)) %>% #current year
  dplyr::ungroup() %>% 
  dplyr:: rename(kelp.year = YEAR) %>%  #rename YEAR to match nppsem df
  dplyr::filter(kelp.year != "2001") %>% 
  dplyr::filter(kelp.year != "2002") %>% 
  dplyr::filter(kelp.year != "2020") %>% 
  dplyr::filter(kelp.year != "2024") %>% 
  dplyr::arrange(SITE) %>% 
  dplyr::ungroup()

#add to df
npp_biomass_variables <- npp_biomass_variables %>% 
  mutate(SAND_COVER = Sand.df$SAND_COVER) 


# Temperature -------------------------------------------------------------

#copy dataframe 
tempdata <- temp_by.10min_dat2

#calculate daily N mean & median
temp_data <- tempdata %>% #tempdata is from nitrate- just a copied df
  separate(DATE_LOCAL, sep="-", into = c("YEAR", "MONTH", "DAY"))  #separate Year-Month-Day into separate columns

temp_data$MONTH <- as.integer(temp_data$MONTH)
temp_data$YEAR <- as.integer(temp_data$YEAR)
temp_data$DAY <- as.integer(temp_data$DAY)

#NA for < 357 data days for temp per year. 
library(dplyr)
library(tidyr)

# Step 1: Daily means (full year)
daily_temps <- temp_data %>% 
  dplyr::filter(SITE %in% c("AQUE", "MOHK", "ABUR")) %>%
  dplyr::select(SITE, YEAR, MONTH, DAY, TEMP_C) %>%
  dplyr::group_by(SITE, YEAR, MONTH, DAY) %>%
  dplyr::summarise(TEMP_daily_MEAN = mean(TEMP_C, na.rm = TRUE), .groups = "drop")

# Step 2: Count total days per SITE/YEAR (before filtering to months)
total_days_df <- daily_temps %>%
  group_by(SITE, YEAR) %>%
  summarise(total_days = n(), .groups = "drop")

# Step 3: Filter to April–August and count high temp days
recruit_temp_df <- daily_temps %>%
  filter(MONTH > 3 & MONTH < 9, YEAR != 2020, YEAR != 2002, YEAR != 2024) %>%
  group_by(SITE, YEAR) %>%
  summarise(
    days_above_19 = sum(TEMP_daily_MEAN >= 19, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Join total_days and apply condition
temp_threshold_day_recruit <- left_join(recruit_temp_df, total_days_df, by = c("SITE", "YEAR")) %>%
  mutate(
    TEMP_THRESHOLD_DAY = case_when(
      total_days >= 357 ~ days_above_19,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(SITE, YEAR, TEMP_THRESHOLD_DAY) %>%
  dplyr:: rename(kelp.year = YEAR) %>%  #rename YEAR to match nppsem df
  dplyr::filter(kelp.year != "2002") %>%  #remove 2002
  dplyr::filter(kelp.year != "2020") %>%   #remove 2002
  dplyr::filter(kelp.year != "2024") %>%   #remove 2002
  arrange(SITE)

npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(TEMP_THRESHOLD_DAY = temp_threshold_day_recruit$TEMP_THRESHOLD_DAY)

# Annual Density ----------------------------------------------------------

#copy df
kelp_frond.data <- kelp.frond.dens_dat %>% 
  separate(DATE, sep="-", into = c("Year", "Month", "Day")) #separate Year-Month-Day into separate columns

kelp_frond.data$Month <- as.numeric(kelp_frond.data$Month)

npp_transect_area <- kelp_frond.data %>%
  dplyr::select(Year, Month, SITE, TRANSECT, SIDE, F_1M, MEAN_F, AREA) %>% 
  unite(col='TSIDE', c('TRANSECT', 'SIDE'), sep="") %>% #rename each transect/side combo a unique name
  dplyr::group_by(SITE, Year, TSIDE) %>% 
  dplyr:: summarise(AREA_AVE = mean(AREA)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(SITE, Year) %>% 
  dplyr::summarise(AREA_TOTAL = sum(AREA_AVE)) %>% 
  dplyr::ungroup() %>% 
  dplyr::arrange(SITE) %>% 
  dplyr::filter(Year != "2002") %>% 
  dplyr::filter(Year != "2020") %>% 
  dplyr::filter(Year != "2024") 

#april
ANNUAL_DENSITY <- kelp_frond.data %>%
  dplyr::select(Year, MONTH, SITE, TRANSECT, SIDE, F_1M) %>% 
  dplyr::filter(F_1M >0) %>% 
  na.omit() %>% 
  dplyr::group_by(SITE, Year) %>% 
  dplyr:: summarise(PLANT_COUNT_TOTAL = length(F_1M)) %>% #number of plants that meet the conditions in each month (i.e. density)
  dplyr::ungroup() %>% 
  complete(SITE, Year, fill = list(PLANT_COUNT_TOTAL = 0)) %>% 
  dplyr::filter(Year != "2020") %>% 
  dplyr::filter(Year != "2002") %>% 
  dplyr::filter(Year != "2024") %>% 
  dplyr::arrange(SITE) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(AREA_TOTAL = npp_transect_area$AREA_TOTAL) %>% #add area column to density df
  dplyr::mutate(PLANT_DENSITY_M2 = PLANT_COUNT_TOTAL/AREA_TOTAL) %>% 
  dplyr::ungroup()


#add df to sem df
npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(PLANT_DENSITY_M2 = ANNUAL_DENSITY$PLANT_DENSITY_M2) 

# Plant Size --------------------------------------------------------------


#copy df
kelp_frond.data <- kelp.frond.dens_dat %>% 
  separate(DATE, sep="-", into = c("Year", "Month", "Day")) #separate Year-Month-Day into separate columns

kelp_frond.data$Month <- as.numeric(kelp_frond.data$Month)


PLANT_SIZE <- kelp_frond.data %>%
  dplyr::select(Year, Month, Day, SITE, F_1M, MEAN_F) %>% 
  dplyr::filter(F_1M >0) %>% 
  dplyr::group_by(SITE, Year, Month, Day) %>% 
  dplyr:: summarise(PLANT_SIZE = mean(F_1M)) %>% #number of plants that meet the conditions in each month (i.e. density)
  dplyr::filter(Year != "2002") %>% 
  dplyr::filter(Year != "2020") %>% 
  dplyr::filter(Year != "2024") %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(SITE, Year) %>% 
  dplyr::summarise(PLANT_SIZE = mean(PLANT_SIZE)) %>% 
  dplyr::arrange(SITE) %>% 
  dplyr::ungroup()

#add df to sem df
npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(PLANT_SIZE = PLANT_SIZE$PLANT_SIZE)

# Annual Temperature -------------------------------------------------------------


# Copy tempdata
tempdata <- temp_by.10min_dat2

# Step 0: Prepare the date parts
temp_data <- tempdata %>%
  separate(DATE_LOCAL, sep = "-", into = c("YEAR", "MONTH", "DAY")) %>%
  mutate(
    MONTH = as.integer(MONTH),
    YEAR = as.integer(YEAR),
    DAY = as.integer(DAY)
  )

# Step 1: Calculate daily temperature means (for full year, no month filtering yet)
daily_temps <- temp_data %>% 
  dplyr::filter(SITE %in% c("AQUE", "MOHK", "ABUR")) %>%
  dplyr::select(SITE, YEAR, MONTH, DAY, TEMP_C) %>%
  dplyr::group_by(SITE, YEAR, MONTH, DAY) %>%
  summarise(TEMP_daily_MEAN = mean(TEMP_C, na.rm = TRUE), .groups = "drop")

# Step 2: Count total days per SITE/YEAR
total_days_df <- daily_temps %>%
  group_by(SITE, YEAR) %>%
  summarise(total_days = n(), .groups = "drop")

# Step 3: Count number of high temperature days (no month filter!)
high_temp_days_df <- daily_temps %>%
  filter(YEAR != 2002, YEAR != 2020, YEAR != 2024) %>%
  group_by(SITE, YEAR) %>%
  summarise(
    days_above_19 = sum(TEMP_daily_MEAN >= 19, na.rm = TRUE),
    .groups = "drop"
  )

# Step 4: Join total days and apply NA condition
temp_threshold_day_annual <- left_join(high_temp_days_df, total_days_df, by = c("SITE", "YEAR")) %>%
  mutate(
    TEMP_THRESHOLD_DAY = case_when(
      total_days >= 357 ~ days_above_19,
      TRUE ~ NA_real_
    )
  ) %>%
  dplyr::select(SITE, YEAR, TEMP_THRESHOLD_DAY) %>%
  rename(kelp.year = YEAR) %>%
  arrange(SITE)

#add to df
npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(TEMP_THRESHOLD_DAY_annual = temp_threshold_day_annual$TEMP_THRESHOLD_DAY) 


# Annual Nitrate Threshold ------------------------------------------------

# STEP 1: Filter original dataset FIRST and create NITRATE column
Nitrate_data <- temp_by.10min_dat2 %>%
  dplyr::filter(SITE == "AQUE" | SITE == "MOHK" | SITE == "ABUR") %>% 
  mutate(NITRATE = case_when(
    SITE == "AQUE" & TEMP_C <= 15.50 ~ -0.00628747 * (TEMP_C^4) + 0.32811 * (TEMP_C^3) - 5.7165 * (TEMP_C^2) + 33.833 * TEMP_C - 9.7322,
    SITE == "AQUE" & TEMP_C > 15.50 ~ 0.22,
    SITE %in% c("MOHK", "ABUR") ~ 2.8e4 * exp(-0.64 * TEMP_C)
  )) %>%
  separate(DATE_LOCAL, into = c("YEAR", "MONTH", "DAY"), sep = "-", remove = FALSE) %>%
  mutate(
    YEAR = as.integer(YEAR),
    MONTH = as.integer(MONTH),
    DAY = as.integer(DAY),
    DATE = as.Date(paste(YEAR, MONTH, DAY, sep = "-")),
    kelp.year = ifelse(MONTH >= 3, YEAR, YEAR - 1)
  )

# STEP 2: Get filtered date range
min_date <- min(Nitrate_data$DATE, na.rm = TRUE)
max_date <- max(Nitrate_data$DATE, na.rm = TRUE)

# STEP 3: Build full daily grid ONLY for filtered sites
all_dates <- tidyr::crossing(
  SITE = unique(Nitrate_data$SITE),  # only AQUE, MOHK, ABUR now
  DATE = seq(min_date, max_date, by = "day")
) %>%
  mutate(
    YEAR = year(DATE),
    MONTH = month(DATE),
    DAY = day(DATE),
    kelp.year = ifelse(MONTH >= 3, YEAR, YEAR - 1)
  )

# STEP 4: Compute daily means
nitrate_daily <- Nitrate_data %>%
  group_by(SITE, DATE, kelp.year) %>%
  summarise(NITRATE_daily_MEAN = mean(NITRATE, na.rm = TRUE), .groups = "drop")

# STEP 5: Join daily means to the full calendar
nitrate_joined <- all_dates %>%
  left_join(nitrate_daily, by = c("SITE", "DATE", "kelp.year"))

# STEP 6: Summarize days < 1 for each site and kelp year
Nitrate_threshold_kelp_year <- nitrate_joined %>%
  group_by(SITE, kelp.year) %>%
  summarise(
    days_below_threshold = sum(NITRATE_daily_MEAN < 1, na.rm = TRUE),
    total_days_with_data = sum(!is.na(NITRATE_daily_MEAN)),
    .groups = "drop"
  ) %>%
  mutate(
    days_below_threshold = ifelse(total_days_with_data < 357, NA, days_below_threshold)
  ) %>%
  dplyr::filter(kelp.year != "2002") %>% 
  dplyr::filter(kelp.year != "2020") %>% 
  dplyr::filter(kelp.year != "2024") %>% 
  arrange(SITE, kelp.year) %>% 
  dplyr::filter(SITE == "AQUE" | SITE == "MOHK" | SITE == "ABUR")


npp_biomass_variables <- npp_biomass_variables %>% 
  dplyr::mutate(NITRATE_THRESHOLD_ANNUAL = Nitrate_threshold_kelp_year$days_below_threshold)


