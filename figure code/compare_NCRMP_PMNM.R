#####################################
### 2024 NWHI
### Pooling site level data to the program level
### comparing NCRMP and PMNM data
#####################################
# set up
rm(list=ls())
library(readr)
#library(rgdal)
library(ggplot2)
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library includes the cast() function used below
library(tidyr)

# LOAD FUNCTIONS FOR DATA CLEANING
source("D:/CRED/GitHub/fish-paste/lib/core_functions.R")
source("D:/CRED/GitHub/fish-paste/lib/fish_team_functions.R")
source("D:/CRED/GitHub/fish-paste/lib/Islandwide Mean&Variance Functions.R")
library(dplyr)

# load combined site data
load("D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/data/Data Outputs/working_site_data.rdata")
head(wsd)
names(wsd)

# filter for NWHI islands that we both surveyed
ncrmp<-wsd %>% filter(PROGRAM == "NCRMP");unique(ncrmp$ISLAND)
wd<-wsd %>% filter(ISLAND == "French Frigate"|ISLAND == "Lisianski"|ISLAND =="Kure"|ISLAND =="Pearl & Hermes")
unique(wd$PROGRAM)
# ### calculate differences in means and 95% CI
# create function to calculate standard error
se<-function(x) sqrt(var(x)/length(x))

# create a function to calculate difference of means and 95% CI
#=c("PISCIVORE", "PLANKTIVORE","PRIMARY","SECONDARY","TotFish","0_20","20_50" ,"50_plus" ,"MEAN_SIZE" ,"P10_30","P30_plus","Scaridae","Carangidae", "Acanthuridae","Serranidae","Labridae","Pomacentridae","CAIG","CAGA","LUKA","CHSL")
calc_spatial<-function(wd, FIELDS_OF_INTEREST){  
  #wd<-bm[bm$OBS_YEAR==2014,]
  m<-aggregate(wd[,FIELDS_OF_INTEREST], by=list(wd[,c("PROGRAM")]), mean)
  stdev<-aggregate(wd[,FIELDS_OF_INTEREST], by=list(wd[,c("PROGRAM")]), sd)
  stderr<-aggregate(wd[,FIELDS_OF_INTEREST], by=list(wd[,c("PROGRAM")]), se)
  n<-aggregate(wd[,FIELDS_OF_INTEREST], by=list(wd[,c("PROGRAM")]), length)
  
  # ## TRYING WITH PLYR
  # g<-wd %>% filter(PROGRAM == "GLTMP")
  # gsum<-group_by()
  # all<-wd %>% group_by(PROGRAM) %>% 
  #   summarise(PISC_avg=mean(PISCIVORE),PISC_sd=sd(PISCIVORE),PISC_n=n_distinct(SITE))
  
  all<-as.data.frame(cbind(t(m[,FIELDS_OF_INTEREST]), t(stdev[,FIELDS_OF_INTEREST]), t(stderr[,FIELDS_OF_INTEREST]), t(n[,FIELDS_OF_INTEREST])));all
  names(all)<-c(paste("m",c("PMNM","NCRMP"), sep="_"), paste("sd",c("PMNM","NCRMP"), sep="_"), paste("se",c("PMNM","NCRMP"), sep="_"), paste("n",c("PMNM","NCRMP"), sep="_"))
  all$DIFF<-all$m_PMNM-all$m_NCRMP
  SE_SCALER<-abs(qt(0.05/2, (all$n_PMNM[1]+all$n_NCRMP[1])-2))   
  se.diff <- sqrt((all$se_PMNM^2) + (all$se_NCRMP^2))
  all$CI95<-se.diff*SE_SCALER
  all$loCI<-all$DIFF-all$CI95
  all$hiCI<-all$DIFF+all$CI95
  
  return(a=all[,c("m_PMNM", "sd_PMNM", "n_PMNM", "m_NCRMP", "sd_NCRMP", "n_NCRMP", "DIFF", "loCI", "hiCI")])	
} # calc_spatial

#Calculate Spatial Pattern
FIELDS<-c("PISCIVORE", "PLANKTIVORE","PRIMARY","SECONDARY","TotFish","0_20","20_50" ,"50_plus" ,"MEAN_SIZE" ,"P10_30","P30_plus","Scaridae","Carangidae", "Acanthuridae","Serranidae","Labridae","Pomacentridae","CAIG","CAGA","LUKA","CHSL")
a<-calc_spatial(wd, FIELDS)
write.csv(a, file="D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/data/Data Outputs/NCRMP_PMNM_Comparisons.csv")


################################################################
################# POOLING TO STRATA LEVEL ########################
###################################################################
# load base files: strata areas, survey master, and raw fish data
# get strata and sectors data (NOTE: data in raw file should be updated)
sectors<-read.csv("D:/CRED/GitHub/fish-paste/data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
# sec2<-read.csv("D:/CRED/Guam/MP analysis/data/ASAN_areas.csv") # load sector areas for Asan, a new sector
# 
# setdiff(names(sectors),names(sec2)) # check that dfs have same column headings
# setdiff(names(sec2),names(sectors))
# # change bPROTECTION_DS to bPROTECTION
# sec2<-sec2 %>% dplyr::rename(bPROTECTION = bPROTECTION_DS)
# sectors<-rbind(sectors,sec2) # combine dfs

## ADD STRATA FIELD
wd$STRATA<-paste(substring(wd$REEF_ZONE,1,1), substring(wd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")

### pooling data  ###############################
wd$ANALYSIS_SEC<-wd$SEC_NAME
# wsd$STRATA<-wsd$ANALYSIS_STRATA

SPATIAL_POOLING_BASE<-c("REGION", "ISLAND", "SEC_NAME","STRATA", "REEF_ZONE", "PROGRAM")    
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, "OBS_YEAR")

### LOOK AT REPLICATION WITHIN STRATA - TO EYEBALL WHETHER THERE ARE STRATA WITHOUT REPLICATION # KM drop strata with no replication? 
tmp<-aggregate(wd[,"METHOD"], by=wd[,c("REGION", "ISLAND", "SEC_NAME","STRATA", "REEF_ZONE","OBS_YEAR", "PROGRAM" ,"SITE")], length)
tmp<-aggregate(tmp[,"x"], by=tmp[,c(POOLING_LEVEL)], length)
#tmp<-merge(sectors, tmp[,c("OBS_YEAR", "ANALYSIS_SEC", "ANALYSIS_STRATA","x")],by=c("ANALYSIS_SEC", "ANALYSIS_STRATA"),all.y=TRUE)
names(tmp)[names(tmp)=="x"]<-"n_sites"
a<-cast(tmp, OBS_YEAR + REGION + ISLAND+SEC_NAME ~ STRATA, value="n_sites", sum, fill=NA)
a

# !!! GET AREA OF ASAN #####

#clean up the sectors table so pool all sub sectors within a scheme into a total for this scheme's sectors
sectors<-aggregate(sectors[,"AREA_HA_2024"], by=sectors[,c("REGION", "ISLAND", "SEC_NAME","STRATA", "REEF_ZONE")], sum)
names(sectors)[names(sectors)=="x"]<-"AREA_HA_2024"

# merge wsd and sector area
wsda<-wd %>% left_join(sectors %>% select(SEC_NAME,STRATA,AREA_HA_2024), by=c("SEC_NAME","STRATA"))

wsda %>% distinct(SEC_NAME,STRATA,AREA_HA_2024)
wsd<-wsda

#################################################################################################################################
############################################# NOW DO THE CALCULATION OF WINHIN-STRATA AND POOLED UP DATA VALUES #################
#################################################################################################################################

# get rid of columns w/ NAN
# data.cols<-c( "PISCIVORE","PLANKTIVORE","PRIMARY","SECONDARY","TotFish","DEPTH","HARD_CORAL","MA" , "CCA","SAND", "0_20","20_50","50_plus","MEAN_SIZE","10_35","35_plus")
data_cols<-c( "PISCIVORE", "PLANKTIVORE","PRIMARY","SECONDARY","TotFish","0_20","20_50" ,"50_plus" ,"MEAN_SIZE" ,"P10_30","P30_plus","Scaridae","Carangidae", "Acanthuridae","Serranidae","Labridae","Pomacentridae","CAIG","CAGA","LUKA","CHSL" )

ADDITIONAL_POOLING_BY<-c("OBS_YEAR", "METHOD","PROGRAM")   # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)
#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
BASE_DATA_COLS<-c("ISLAND", "STRATA", "OBS_YEAR", "METHOD","SITEVISITID", data_cols)
data.per.strata<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA_2024"))
a<-as.data.frame(data.per.strata)
write.csv(a,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/data/Data Outputs/tmp strata dataCOMPARE.csv", row.names = F)
save(a,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/data/Data Outputs/tmp strata dataCOMPARE.Rdata")

