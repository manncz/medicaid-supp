###################################################################################################
#Script: 01-base-health-ahrf.R
#Inputs: data-raw/AHRF2019.asc
#Outputs: data/base_ahrf_cnty.csv
#         data/temp/cnty_fips.csv
#Author: CM
#Date: 6/17/2020
###################################################################################################

library(dplyr)
library(tidyr)
library(readr)

###################################################################################################
ahrf_raw  <- paste0("../data-raw/", "AHRF2019.asc") %>%
  read_fwf(fwf_cols(stateFIPS = c(122,123), # 'state FIPS'
                    cntyFIPS = c(124,126), # 'county FIPS'
                    stateName = c(46,64),
                    cntyName = c(67,91),
                    pop_2013 = c(16457,16464), # 'population estimate'
                    pop_2012 = c(16465,16472),
                    pop_2011 = c(16473,16480),
                    pop_2010 = c(16481,16488), # 'census population'
                    pctUrban_2010 = c(19687,19690), # 'Percent urban population'
                    vetPop_2013 = c(19803,19808), # 'veteran population estimate'
                    vetPop_2012 = c(19809,19814),
                    vetPop_2011 = c(19815,19820),
                    mdcaidElig_2010 = c(20598,20605), # 'medicaid eligible (total)'
                    mdcaidElig_2011 = c(20590,20597),
                    mdcaidElig_2012 = c(20582,20589),
                    medIncome_2013 = c(23219,23224), # 'median household income'
                    medIncome_2012 = c(23225,23230),
                    medIncome_2011 = c(23231,23236),
                    pctPovty_2013 = c(24094,24097), # 'percent persons in poverty'
                    pctPovty_2012 = c(24098,24101),
                    pctPovty_2011 = c(24102,24105),
                    snap_2013 = c(24667,24673), # 'food stamp/SNAP recipients (estimate)'
                    snap_2012 = c(24674,24680),
                    snap_2011 = c(24681,24687),
                    pctNoIns_2013 = c(26284,26287), # '% 18-64 without health insurance'     
                    pctNoIns_2012 = c(26288,26291),
                    pctNoIns_2011 = c(26292,26295),
                    unplmtRate_2013 = c(30893,30895), # '16+ unemployment rate'
                    unplmtRate_2012 = c(30896,30898),
                    unplmtRate_2011 = c(30899,30901),
                    # pctGoodAir_2013 = c(31640,31644), # '% good air quality days'
                    # pctGoodAir_2012 = c(31645,31649),
                    # pctGoodAir_2011 = c(31650,31654),
                    avgPM25_2011 = c(31655,31659), # 'average daily PM2.5'
                    popDens_2010 = c(31521, 31526) # 'population density'
  ),
  progress=FALSE
  ) 

#replace the PM2.5 variable with that from 2020
ahrf_raw_2020  <- paste0("../data-raw/AHRF2020/", "AHRF2020.asc") %>%
  read_fwf(fwf_cols(stateFIPS = c(122,123), # 'state FIPS'
                    cntyFIPS = c(124,126), # 'county FIPS'
                    stateName = c(46,64),
                    cntyName = c(67,91),
                    avgPM25check_2011 = c(31757,31761),
                    avgPM25_2013 = c(31747,31751) # 'average daily PM2.5'
  ),
  progress=FALSE
  ) 

ahrf <- ahrf_raw %>%
  left_join(ahrf_raw_2020, by = c("stateFIPS", "cntyFIPS", "stateName", "cntyName")) %>%
  mutate(FIPS=paste0(sprintf('%s', stateFIPS), sprintf('%s', cntyFIPS))) %>%
  mutate_at(vars(pop_2013:avgPM25_2013), function(x){as.numeric(x)}) %>%
  mutate_at(vars(matches("pct"), matches("unplmt"), matches("popDens"), matches("PM25")), function(x){x/10}) %>%
  mutate(avgPM25_2011 = avgPM25_2011/10) %>%
  filter(cntyName != "State of Alaska") %>%
  dplyr::select(FIPS, everything()) #reorder so FIPS is first variable

#check whether counties included align between AHRF and mortality data
base_cnty <- read.csv("../data/base_cnty.csv", colClasses = c("FIPS"="character") )
ahrf.fips <- ahrf$FIPS
mort.fips <- base_cnty$FIPS

#fips codes in the mortality data not in AHRF
mort.fips[which(!(mort.fips %in% ahrf.fips))]

# There are two counties in the mortality data that don't match with the AHRF data
# 02270	Wade Hampton	AK
# 46113	Shannon	SD

# FROM THE AHRF DOCUMENTATION:
# 1)	Effective July 1, 2015, Wade Hampton Census Area, Alaska (02270) was changed to 
# Kusilvak Census Area, Alaska (02158). 
# This change was made with the 2015-2016 release of the AHRF.
# 
# 2)	Effective May 1, 2015, Shannon County, South Dakota (46113) was changed to 
# Oglala Lakota County, South Dakota (46102). 
# This change was made with the 2015-2016 release of the AHRF.

# In the documentation we can see that these counties are just indicated with different
# FIPS county codes in the AHRF data, so we change the AHRF data FIPS to match the mortality data

ahrf$FIPS[which(ahrf$FIPS == "02158")] <- "02270" # Wade Hampton AK
ahrf$FIPS[which(ahrf$FIPS == "46102")] <- "46113" # Shannon SD

ahrf.fips <- ahrf$FIPS
# this is primarily guam, puerto rico,and the US virgin islands, but there are 5 counties in Alaska in addition
# that are in the AHRF data and not in the mortality data
check <- ahrf %>% filter(!(FIPS %in% mort.fips)) 
check2 <- ahrf %>% filter((FIPS %in% mort.fips))

#rename state and county name for district of columbia to help match with other data
ahrf$stateName[which(ahrf$stateName == "Dist. of Columbia")] <- "District of Columbia"
ahrf$cntyName[which(ahrf$stateName == "District of Columbia")] <- "District of Columbia"

#there are these alaska counties that aren't in the mortality data, keep them for now
ahrf <- ahrf %>%
  filter((FIPS %in% mort.fips) | stateName == "Alaska")

#calculate % of population for necessary variables.
#this also sets up code incase we want to summarize over years
incnames <- colnames(ahrf)
ahrf_clean <- ahrf %>%
  pivot_longer(cols = pop_2013:avgPM25_2013,
               names_to = c("var", "year"),
               names_pattern = "(.*)_(.*)") %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(vetPop = vetPop/pop*100, mdcaidElig = mdcaidElig/pop*100, snap = snap/pop*100) %>%
  pivot_wider(names_from = year, values_from = c(pop:avgPM25check), names_glue = "{.value}_{year}") %>%
  dplyr::select(all_of(incnames))

# save data as .csv to merge
ahrf_clean %>% write_csv("../data/base_ahrf_cnty.csv")

#save list of FIPS and county names for use with other files
ahrf %>% dplyr::select(FIPS:cntyName) %>%
  mutate(cntyName = str_to_lower(cntyName),
         stateName = str_to_lower(stateName)) %>%
  write_csv("../data/temp/cnty_fips.csv")


############################### CHECKING MISSING DATA ###############################

#only consider counties that are in the mortality data
for (x in names(ahrf)){
  print(paste0(x, " is missing ", sum(is.na(ahrf[,x])), " obs"))
}

# considering all variables other than medicaid eligible 2011 and 2012, we have complete data for all 
# but 33 counties, which these are almost all in alaska and hawaii, but
# bedford city virginia is also missing snap 2013

cc.id <- check.mi %>% select(-mdcaidElig_2011, -mdcaidElig_2012) %>% complete.cases()
check <- check.mi[!cc.id,]

# the pm25 data is missing for all counties in Alaska and Hawai.
check2 <- check.mi %>% filter(stateName %in% c("Alaska", "Hawaii"))

# excluding the pm25 variable, we are only missing data for these three counties in Alaska and one in Hawaii and virginia
cc.id <- check.mi %>% select(-mdcaidElig_2011, -mdcaidElig_2012, -avgPM25_2011) %>% complete.cases()
check3 <- check.mi[!cc.id,]; check.mi[!cc.id,] %>% select(stateName, cntyName)

####################################################################################
