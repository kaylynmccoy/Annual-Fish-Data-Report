rm(list=ls())
# load data 
load("data/Data Outputs/raw_working_data.rdata")
# load library functions
source("data/fish_team_functions.R")
source("data/Islandwide Mean&Variance Functions.R")
library(reshape)
## need to read in the species_table from Fish Base
#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

## using Calc_REP functions (from fish_team_functions) to get richness and biomass estimates per replicate....
r1<-Calc_REP_Bio(wd, "FAMILY")
# #drop level UNKNOWN
r1<-r1 %>% dplyr::select(-"UNKNOWN")
family.cols<-names(r1)[7:dim(r1)[2]]
r1$TotFish<-rowSums(r1[,family.cols])

r2<-Calc_REP_Species_Richness(wd)

UNIQUE_SURVEY<-c("SITE", "SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")
SURVEY_SITE_DATA<-c("DEPTH")

r3<-Calc_REP_nSurveysArea(wd, UNIQUE_SURVEY,UNIQUE_REP,  UNIQUE_COUNT,SURVEY_SITE_DATA)

r3<-Calc_REP_nSurveysArea(wd, c("SITE", "SITEVISITID","METHOD"), c("SITE", "SITEVISITID","METHOD","REP"), c("SITE", "SITEVISITID","METHOD","REP","REPLICATEID"), c("SITE", "SITEVISITID","METHOD","REP","REPLICATEID","DEPTH"))

COMPARE_ON<-c("SITEVISITID", "SITE", "REP", "REPLICATEID")
compdata<-merge(r1[,c(COMPARE_ON, "TotFish")], r2[,c(COMPARE_ON, "SPECIESRICHNESS")], by=COMPARE_ON, all.x=T)
compdata<-merge(compdata[,c(COMPARE_ON, "TotFish", "SPECIESRICHNESS")], r3[,c(COMPARE_ON)], by=COMPARE_ON, all.x=T)

## get year, region and island data
a<-unique(wd[,c("SITE","SITEVISITID","OBS_YEAR","ISLAND","REGION","REPLICATEID","DIVER","ANALYSIS_YEAR")])

test<-merge(compdata, a, by="REPLICATEID", all.x=T) ## this collates year, region etc. with the indiv diver ests per rep
compdata<-test

## need to rename some of the cols after the merge
names(compdata)<-c("REPLICATEID","SITEVISITID","SITE","REP","TotFish","SPECIESRICHNESS",
                   "SITE.y","SITEVISITID.y","OBS_YEAR","ISLAND","REGION","DIVER","ANALYSIS_YEAR")

# set wd
setwd("/figures/appendix")

## divervsdiver4 - creates an anonymous and named version of diver comparisons for totfish, richness and coral estimates 
##- need to look at the range of the data per year region to tweak the x axis range
dataAS<-compdata[compdata$ANALYSIS_YEAR==2024,]
summary(dataAS$TotFish)# look at max value for tot fish and adjust x_range below - may need to adjust for extreme outliers

# RENAME REGION TO AS
unique(dataAS$REGION)
## to create a multigraph with total fish, richness and coral estimates run divervsdiver3
divervsdiver4(data=dataAS, year = "2024", region="MHI", x_range= 150)
divervsdiver4(d)

###################################################################3 OLD CODE ###############
# load library functions
source("D:/CRED/GitHub/fish-paste/lib/core_functions.R")
source("D:/CRED/GitHub/fish-paste/lib/fish_team_functions.R")
source("D:/CRED/GitHub/fish-paste/lib/Islandwide Mean&Variance Functions.R")

##### Surveys per region per year- Table A 1. ######
library(dplyr)
#library(tidyr)
load("data/Data Outputs/working_site_data.rdata")

# make a pivot table
#detach(package:plyr)
site<-wsd %>% group_by(REGION, OBS_YEAR, ISLAND,METHOD) %>% 
  count()
haw<-as.data.frame(site) %>% filter(ISLAND == "Hawaii") %>% dplyr::select(-c(REGION,ISLAND,METHOD))%>% dplyr::rename(Year = OBS_YEAR, Sites = n)
#a<-haw %>% pivot_wider(names_from = OBS_YEAR, values_from = n) 

#save file
write.csv(site,"D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/tables/appendixA_surveys_per_method_per_region_per_year.csv",row.names = F)

# appendix 4: strata sector by year ---------------------------------------
# these show sites at the level they were pooled for analysis. Backreef and ROSE lagoon sites are all pooled. Drop sectors that have 1 site, these were dropped for analysis
library(reshape)
library(dplyr)
getwd()
load("D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/data/Data Outputs/sites_year_reef_zone_depth_bin.Rdata") # loads as 'a'
wd<-a
head(wd)
# get 2023 sites
wd<-wd[wd$ANALYSIS_YEAR=="2024",]
# # combine Guam MPs into one sector
# wd$SEC_NAME <-dplyr::recode(wd$SEC_NAME, GUA_ACHANG = "GUAM_MP", GUA_PITI_BOMB = "GUAM_MP", GUA_PATI_POINT = "GUAM_MP",GUA_TUMON= "GUAM_MP",GUA_EAST_OPEN = "GUAM_OPEN",GUA_WEST_OPEN="GUAM_OPEN")

detach(package:plyr)
#summarize sites by SEC_NAME
wd<-wd %>% dplyr::group_by(REGION, ISLAND, SEC_NAME) %>% summarize(FD= sum(FD),FM=sum(FM),FS=sum(FS))

# rename columns
wd<-wd %>% dplyr::rename(Region = REGION, Island = ISLAND, Sector=SEC_NAME, Deep=FD,Mid=FM,Shallow=FS )

write.csv(wd,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/tables/sites_year_reef_zone_depth_bin2024.csv", row.names = F)
# wd$Forereef<-(wd$FD+wd$FM+wd$FS)
# wd$Lagoon<-(wd$LD+wd$LM+wd$LS) # should equal LA = LAGOON ALL?
# wd$protected_slope<-(wd$PD+wd$PM+wd$PS)
# wd$Backreef<-wd$BA
# wd$all_sites<-(wd$Forereef+wd$Lagoon+wd$LA+wd$protected_slope+wd$Backreef)


write.csv(wd,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2023_status_report/AS/tables/appendix_4.csv", row.names = F)

# appendix 5: diver vs diver comparisons ----------------------------------
getwd()
load("data/Data Outputs/raw_working_data.rdata")

## need to read in the species_table from Fish Base
#Pull all species information into a separate df, for possible later use ..
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)

## using Calc_REP functions (from fish_team_functions) to get richness and biomass estimates per replicate....
r1<-Calc_REP_Bio(wd, "FAMILY")
# #drop level UNKNOWN
r1<-r1 %>% dplyr::select(-"UNKNOWN")
family.cols<-names(r1)[7:dim(r1)[2]]
r1$TotFish<-rowSums(r1[,family.cols])

r2<-Calc_REP_Species_Richness(wd)

UNIQUE_SURVEY<-c("SITE", "SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")
SURVEY_SITE_DATA<-c("DEPTH")

r3<-Calc_REP_nSurveysArea(wd, UNIQUE_SURVEY, UNIQUE_REP, UNIQUE_COUNT, SURVEY_SITE_DATA)

COMPARE_ON<-c("SITEVISITID", "SITE", "REP", "REPLICATEID")
compdata<-merge(r1[,c(COMPARE_ON, "TotFish")], r2[,c(COMPARE_ON, "SPECIESRICHNESS")], by=COMPARE_ON, all.x=T)
compdata<-merge(compdata[,c(COMPARE_ON, "TotFish", "SPECIESRICHNESS")], r3[,c(COMPARE_ON)], by=COMPARE_ON, all.x=T)


## get year, region and island data
a<-unique(wd[,c("SITE","SITEVISITID","OBS_YEAR","ISLAND","REGION","REPLICATEID","DIVER","ANALYSIS_YEAR")])

test<-merge(compdata, a, by="REPLICATEID", all.x=T) ## this collates year, region etc. with the indiv diver ests per rep
compdata<-test

## need to rename some of the cols after the merge
names(compdata)<-c("REPLICATEID","SITEVISITID","SITE","REP","TotFish","SPECIESRICHNESS",
                   "SITE.y","SITEVISITID.y","OBS_YEAR","ISLAND","REGION","DIVER","ANALYSIS_YEAR")


# set wd
setwd("D:/CRED/fish_cruise_routine_report/monitoring_report/2023_status_report/AS/figures/appendix")

## divervsdiver4 - creates an anonymous and named version of diver comparisons for totfish, richness and coral estimates 
##- need to look at the range of the data per year region to tweak the x axis range
dataAS<-compdata[compdata$ANALYSIS_YEAR==2023,]
summary(dataAS)# look at max value for tot fish and adjust x_range below - may need to adjust for extreme outliers

# RENAME REGION TO AS
unique(dataAS$REGION)
## to create a multigraph with total fish, richness and coral estimates run divervsdiver3
divervsdiver4(data=dataAS, year = "2023", region="SAMOA", x_range= 100)



# appendix D: random stratified sites surveyed per region / island  -------------####
library(reshape)
library(tidyr)
library(dplyr)
library(readr)
load("D:/CRED/fish_cruise_routine_report/monitoring_report/2023_status_report/AS/data/Data Outputs/clean_working_site_data_used_in_higher_pooling_for_report.Rdata")
site<-wsd.uncap

# SELECT relevant fields
test<-site %>% dplyr::select("REGION","OBS_YEAR","ISLAND","ANALYSIS_YEAR","SITE") %>% 
  group_by(OBS_YEAR,ISLAND) %>% 
  summarise(n=n())
head(test)
hm<-cast(test, ISLAND~OBS_YEAR)
head(hm)
hm<-hm %>% filter(ISLAND != "South Bank")

# save file
write.csv(hm, file="D:/CRED/fish_cruise_routine_report/monitoring_report/2023_status_report/AS/tables/appendix_nspc_surveys_per_island.csv", row.names = F)

# sectors:
# SELECT relevant fields
test<-site %>% dplyr::select("REGION","OBS_YEAR","ISLAND","SEC_NAME","ANALYSIS_YEAR","SITE") %>% 
  group_by(OBS_YEAR,ISLAND,SEC_NAME) %>% 
  summarise(n=n())
head(test)
hm<-pivot_wider(test,names_from = OBS_YEAR, values_from = n)
head(hm)
hm<-hm %>% mutate(across(everything(), ~ replace_na(as.character(.), '-')))%>% filter(ISLAND != "South Bank")

# save file
write.csv(hm, file="D:/CRED/fish_cruise_routine_report/monitoring_report/2023_status_report/AS/tables/appendix_nspc_surveys_per_sector.csv", row.names = F)


# #### ------all years region averages and se------------------
# load("D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/data/Data Outputs/MONREPdata_pooled_reg_FRF.rdata")
# ref<-as.data.frame(dpR) # forereef only reference
# # get relevant columns with mean and SE
# names(ref)
# test<-ref[,c( "Mean.REGION","Mean.REEF_ZONE","Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE","Mean.TotFish","Mean.0_20" ,"Mean.20_50","Mean.50_plus","Mean.MEAN_SIZE","Mean.P10_30" , "Mean.P30_plus","Mean.Carcharhinidae","Mean.Carangidae", "Mean.N"   ,"PooledSE.PRIMARY","PooledSE.SECONDARY","PooledSE.PLANKTIVORE","PooledSE.PISCIVORE","PooledSE.TotFish","PooledSE.0_20" ,"PooledSE.20_50","PooledSE.50_plus","PooledSE.MEAN_SIZE","PooledSE.P10_30", "PooledSE.P30_plus","PooledSE.Carcharhinidae" ,"PooledSE.Carangidae")]
# # round to 1 decimal place
# names(test)
# temp<-round(test[,c(3:29)],digits=1)
# test<-cbind(test$Mean.REGION,test$Mean.N,temp)
# # rename region
# names(test)[1]<-"Region"
# names(test)[2]<-"Sites"
# 
# # combine mean and SE in one column for each metric
# test$All_fishes<-paste(test$Mean.TotFish," (",test$PooledSE.TotFish,")",sep="")
# test$Piscivores<-paste(test$Mean.PISCIVORE," (",test$PooledSE.PISCIVORE,")",sep="")
# test$Sec_consumers<-paste(test$Mean.SECONDARY," (",test$PooledSE.SECONDARY,")",sep="")
# test$Pri_consumers<-paste(test$Mean.PRIMARY," (",test$PooledSE.PRIMARY,")",sep="")
# test$Planktivores<-paste(test$Mean.PLANKTIVORE," (",test$PooledSE.PLANKTIVORE,")",sep="")
# test$small_cm_TL<-paste(test$Mean.0_20," (",test$PooledSE.0_20,")",sep="")
# test$mid_cm_TL<-paste(test$Mean.20_50," (",test$PooledSE.20_50,")",sep="")
# test$great_cm_TL<-paste(test$Mean.50_plus," (",test$PooledSE.50_plus,")",sep="")
# test$mean_size<-paste(test$Mean.MEAN_SIZE," (",test$PooledSE.MEAN_SIZE,")",sep="")
# test$Mean.P10_30<-paste(test$Mean.P10_30," (",test$PooledSE.P10_30,")",sep="")
# test$Mean.P30_plus<- paste(test$Mean.P30_plus," (",test$PooledSE.P30_plus,")",sep="")
# test$mean.sharks<-paste(test$Mean.Carcharhinidae," (",test$PooledSE.Carcharhinidae,")",sep="")
# test$mean.jacks<-paste(test$Mean.Carangidae," (",test$PooledSE.Carangidae,")",sep="")
# 
# # just keep relevant columns
# colnames(test)
# teste<-test[,c(1,2,30:40)]
# head(teste)
# tests<-rbind(teste[c(2,1,6,5,3,4),])
# 
# write.csv(teste,file="D:/CRED/fish_cruise_routine_report/monitoring_report/2024_status_report/tables/table_2.csv")

# Table for island figures and sites surveyed per year and habitat --------------------------

# # load site data
# library(plyr)
# load("D:/CRED/fish_cruise_routine_report/monitoring_report/2022_status_report/data/Data Outputs/working_site_data.rdata")
# head(wsd)
# summary(wsd)
# unique(wsd$OBS_YEAR)
# 
# # add count column to sum sites
# wsd$N<-1
# # sum by island, reef zone, year
# sum<-ddply(wsd, .(ISLAND,METHOD,OBS_YEAR,REEF_ZONE),summarize,"n_sites"=sum(N))
# head(sum)
# write.csv(sum, file="D:/CRED/fish_cruise_routine_report/monitoring_report/2022_status_report/tables/nspc_surveys_per_island_year_zone.csv")
