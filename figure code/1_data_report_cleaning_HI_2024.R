# LOAD NCRMP FISH DATA, CLEAN AND PREP FOR MONITORING REPORT

# SETUP --------------------
rm(list=ls())

# load packages
library(gdata)             # needed for drop_levels()
library(reshape)           # reshape library inclues the cast() function used below
library(dplyr)

# load library functions
#source("data/core_functions.R")
source("data/fish_team_functions.R")
source("data/Islandwide Mean&Variance Functions.R")

# load base files: strata areas, survey master, and raw fish data
# get strata and sectors data (NOTE: data in raw file should be updated)
sectors<-read.csv("data/Sectors-Strata-Areas.csv", stringsAsFactors=FALSE)
# load site master to merge with sector names
sm<-read.csv("data/SURVEY MASTER.csv")
# load all fish data
load("data/ALL_REA_FISH_RAW.rdata") 
x<-df

# DATA CHECKS --------------
# make sure we have 2024 data
unique(x$OBS_YEAR)

# DATA CLEANING AND FILTERING ------------------------

# select fields we currently use
DATA_COLS<-c("SITEVISITID", "METHOD", "DATE_", "OBS_YEAR",  "SITE", "REEF_ZONE",  "DEPTH_BIN",  "ISLAND", "LATITUDE",  "LONGITUDE",  "REGION" , "REGION_NAME", "EXCLUDE_FLAG", "TRAINING_YN",
"REP",  "REPLICATEID", "DIVER", "HABITAT_CODE", "DEPTH", "SPECIES", "COUNT", "SIZE_", "OBS_TYPE", 
"COMPLEXITY", "SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150", "MAX_HEIGHT", 
"SCIENTIFIC_NAME",  "TAXONNAME", "COMMONNAME", "GENUS", "FAMILY" , "COMMONFAMILYALL", "LMAX", "LW_A",  "LW_B",  "LENGTH_CONVERSION_FACTOR", "TROPHIC", "TROPHIC_MONREP")
x<-x[,DATA_COLS]

# add leading zeros to site number (each site will have 5 numeric digits)
x$SITE<-SiteNumLeadingZeros(x$SITE)

#remove data from certain types of surveys 
x[is.na(x$TRAINING_YN),]$TRAINING_YN<-FALSE   #change NAs to FALSE --> none of the older data was 'training data'
x<-subset(x, x$TRAINING_YN==FALSE) #drop data from training surveys
x<-subset(x, x$EXCLUDE_FLAG==0, drop=TRUE) #drop data flagged as needed to be excluded
x<-subset(x, x$METHOD %in% c("nSPC"), drop=TRUE) #only keep nSPC surveys
x<-subset(x, x$OBS_YEAR >2008, drop=TRUE) #can subset by survey years
x<-subset(x, x$OBS_TYPE %in% c("U","I","N"))# filter for observation type: I = instantaneous; N = non-instantaneous; F = new species seen during 5-10 min window of survey; T = new species seen during 10-30 min window of survey; P = presence (outside cylinder, species of interest)

#add SURVEY MASTER information to dataset  
x<-merge(x, sm[,c("SITEVISITID", "SEC_NAME", "ANALYSIS_YEAR", "ANALYSIS_SCHEME")], by="SITEVISITID", all.x=TRUE)

#CHECK THAT all SEC_NAME are present in the SURVEY MASTER file
test<-x[is.na(x$SEC_NAME)  & x$METHOD=="nSPC", c("REGION", "SITE","OBS_YEAR", "METHOD"),]
if(dim(test)[1]>0) {cat("nSPC sites with MISSING SEC_NAME")}   
test # should be 0

#where there is substrate_height data, calculate avg height and ave_height_variability to standardize complexity metrics (mean height, mean height variability, max height)
sh_out<-CalcMeanSHMeanSHDiff(x)
x$MEAN_SH<-sh_out[[1]]
x$SD_SH_DIFF<-sh_out[[3]]
x<-x[, setdiff(names(x),c("SUBSTRATE_HEIGHT_0", "SUBSTRATE_HEIGHT_20", "SUBSTRATE_HEIGHT_50", "SUBSTRATE_HEIGHT_100", "SUBSTRATE_HEIGHT_150"))] #remove SUBSTRATE_HEIGHT fields
x<-droplevels(x) #drop unused levels

# CLEAN UP NAs 
tmp.lev<-levels(x$HABITAT_CODE); head(tmp.lev)
levels(x$HABITAT_CODE)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$SCIENTIFIC_NAME); head(tmp.lev)
levels(x$SCIENTIFIC_NAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONNAME); head(tmp.lev)
levels(x$COMMONNAME)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$GENUS); head(tmp.lev)
levels(x$GENUS)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$FAMILY); head(tmp.lev)
levels(x$FAMILY)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$COMMONFAMILYALL); head(tmp.lev)
levels(x$COMMONFAMILYALL)<-c(tmp.lev, "UNKNOWN")
tmp.lev<-levels(x$TROPHIC_MONREP); head(tmp.lev)
levels(x$TROPHIC_MONREP)<-c(tmp.lev, "UNKNOWN")

x[is.na(x$HABITAT_CODE),"HABITAT_CODE"]<-"UNKNOWN"
x[is.na(x$SCIENTIFIC_NAME),"SCIENTIFIC_NAME"]<-"UNKNOWN"
x[is.na(x$COMMONNAME),"COMMONNAME"]<-"UNKNOWN"
x[is.na(x$GENUS),"GENUS"]<-"UNKNOWN"
x[is.na(x$FAMILY),"FAMILY"]<-"UNKNOWN"
x[is.na(x$COMMONFAMILYALL),"COMMONFAMILYALL"]<-"UNKNOWN"
x[is.na(x$TROPHIC_MONREP),"TROPHIC_MONREP"]<-"UNKNOWN"

x[is.na(x$COUNT),]$COUNT<-0
x[is.na(x$SIZE_),]$SIZE_<-0
#x[is.na(x$LMAX),]$LMAX<-999
x<-droplevels(x) #drop unused levels

UNIQUE_ROUND<-c("REGION", "OBS_YEAR", "METHOD")
round_table<-Aggregate_InputTable(x, UNIQUE_ROUND)
wd<-droplevels(x) #drop unused levels

# CHECK REGION - NWHI AND MHI
unique(wd$REGION)
wd<-wd %>% filter(REGION=="MHI"|REGION == "NWHI")
wd<-droplevels(wd) #drop unused levels

# remove benthic fields - COMPLEXITY, MAX_HEIGHT, MEAN_SH, SD_SH_DIFF
wd<-wd %>% dplyr::select(-c("COMPLEXITY", "MAX_HEIGHT", "MEAN_SH", "SD_SH_DIFF"))

# ADD PROGRAM FIELD
wd$PROGRAM<-"NCRMP"

### ----------------------------BRING IN MONUMENT DATA TO MERGE--------------
load("data/data outputs/PMNM_DATA_FORMATTED.Rdata")
names(PMNM_DATA_FORMATTED)
names(wd) #colnames should match

test<-rbind(wd,PMNM_DATA_FORMATTED)

# check for NA values
a<-subset(test,is.na(test))

# remove NA values
test<- na.omit(test)
#rename
wd<-test


# OUTPUT raw working data (used to create appendix species list) -------------------------
save(wd, file="data/data outputs/raw_working_data.rdata")

##DEFINE SURVEY IDENTIFIERS 
UNIQUE_SURVEY<-c("SITEVISITID","METHOD")
UNIQUE_REP<-c(UNIQUE_SURVEY, "REP")
UNIQUE_COUNT<-c(UNIQUE_REP, "REPLICATEID")

##ASSIGN/CALCULATE SURVEY METADATA
SURVEY_INFO<-c("OBS_YEAR", "REGION", "REGION_NAME", "ISLAND", "ANALYSIS_SCHEME", "ANALYSIS_YEAR", "SEC_NAME", "SITE", "DATE_", "REEF_ZONE", "DEPTH_BIN", "LATITUDE", "LONGITUDE", "SITEVISITID", "METHOD")
survey_table<-Aggregate_InputTable(wd, SURVEY_INFO)
island_table<-Aggregate_InputTable(wd, c("REGION","ISLAND"))

#SURVEY_SITE_DATA<-c("DEPTH")#"ComplexityValue",

##FORM DATAFRAME OF SITE METADATA 
surveys<-survey_table

## FORM DATAFRAME OF FISH SPECIES INFO (for possible later use)
FISH_SPECIES_FIELDS<-c("SPECIES","TAXONNAME", "FAMILY", "COMMONFAMILYALL", "TROPHIC_MONREP", "LW_A", "LW_B", "LENGTH_CONVERSION_FACTOR")
species_table<-Aggregate_InputTable(wd, FISH_SPECIES_FIELDS)
# save fish species table
save(species_table, file="data/data outputs/species_table.rdata")
write.csv(species_table, file="data/data outputs/species_table.csv")

# SELECT SUMMARY METRICS --------------------------------------------------
# calc pooled site biomass by consumer group, species, and common family
r1<-Calc_Site_Bio(wd, "TROPHIC_MONREP"); trophic.cols<-(c("PISCIVORE","PLANKTIVORE" ,"PRIMARY","SECONDARY"  ))
r4<-Calc_Site_Bio_By_SizeClass(wd, c(0,20,50,Inf)); size.cols<-names(r4)[3:dim(r4)[2]]
r6<-Calc_Site_MeanLength(wd)   # Calcaulte mean fish length for species > 30% of LMax (default set at 30%, min_size=10)
r6[is.na(r6)]<-10      						 # default min size ot 10cm, so we do not have zero size at a site
wdp<-wd; wdp[wdp$FAMILY != "Scaridae",]$COUNT<-0	 #Version of wsd with size (therefore biomass) set to 0 for non parrots
r7<-Calc_Site_Bio_By_SizeClass(wdp, c(10,30,Inf))    #This is parrotfish biomass by Size Class

# families of interest
# change family field from factor to character
wd$FAMILY<-as.character(wd$FAMILY)

wd.fam<-wd %>% mutate_at(vars(SPECIES), ~ifelse(FAMILY %in% c("Carangidae","Sphyrnidae","Carcharhinidae","Ginglymostomatidae"),.,"NONFOCAL")) %>% mutate_at(vars(COUNT, SIZE_), ~ifelse(SPECIES %in% c("NONFISH", "CRYPTIC", "NONFOCAL","FISH"), 0, .)) # NONFOCAL --> TURN COUNTS & SIZES TO 0
r8<-Calc_Site_Bio(wd.fam, "FAMILY") %>% select(c( "SITEVISITID","METHOD","Carangidae","Carcharhinidae")); fam.cols<-c("Carangidae","Carcharhinidae")

# species of interest
# change species field from factor to character
wd$SPECIES<-as.character(wd$SPECIES)

wd.spp<-wd %>% mutate_at(vars(SPECIES),~ifelse(SPECIES %in% c("CEAR", "LUFU", "LUKA", "LUGI","NEMA","CENA" ),.,"NONFOCAL")) %>% mutate_at(vars(COUNT, SIZE_), ~ifelse(SPECIES %in% c("NONFISH", "CRYPTIC", "NONFOCAL","FISH"), 0, .)) # NONFOCAL --> TURN COUNTS & SIZES TO 0
r9<-Calc_Site_Bio(wd.spp, "SPECIES") %>% select("SITEVISITID","METHOD","CEAR","LUFU","LUKA","NEMA" );sp.cols<-c("CEAR","LUFU","LUKA","NEMA")

# # herbivorous surgeons, first turn non-surg to zero, then turn plank to zero
wd.ac<-wd %>% mutate_at(vars(SPECIES),~ifelse(FAMILY %in% c("Acanthuridae"),.,"NONFOCAL")) %>% mutate_at(vars(COUNT, SIZE_), ~ifelse(SPECIES %in% c("NONFISH", "CRYPTIC", "NONFOCAL","FISH"), 0, .)) # NONFOCAL --> TURN COUNTS & SIZES TO 0
wd.ac<-wd.ac %>% mutate_at(vars(SPECIES),~ifelse(TROPHIC_MONREP %in% c("PRIMARY"),.,"NONFOCAL")) %>% mutate_at(vars(COUNT, SIZE_), ~ifelse(SPECIES %in% c("NONFISH", "CRYPTIC", "NONFOCAL","FISH"), 0, .)) # NONFOCAL --> TURN COUNTS & SIZES TO 0
r10<-Calc_Site_Bio(wd.ac, "FAMILY") %>% select(c( "SITEVISITID","METHOD","Acanthuridae")); ac.cols<-c("Acanthuridae")

# #### RAN LATER 
# r2<-Calc_Site_Abund(wd.spp, "SPECIES") %>% dplyr::select("SITEVISITID","METHOD","CEAR","LUFU","LUKA","NEMA" );sp.cols<-c("CEAR","LUFU","LUKA","NEMA")
# invasive_spp_site_abund<-merge(surveys,r2,by=UNIQUE_SURVEY)
# # save file
# save(invasive_spp_site_abund,file="data/data outputs/invasive_spp_site_abund.Rdata")
# # total abund


wsd<-merge(surveys,r1,by=UNIQUE_SURVEY)
wsd$TotFish<-rowSums(wsd[,c("PISCIVORE","PLANKTIVORE","PRIMARY","SECONDARY")])
wsd<-merge(wsd, r4, by=UNIQUE_SURVEY)
names(wsd)[match(c("[0,20]", "(20,50]","(50,Inf]"),names(wsd))] <- c("0_20", "20_50", "50_plus")
wsd<-merge(wsd,r6,by=UNIQUE_SURVEY)
wsd<-merge(wsd,r7,by=UNIQUE_SURVEY, all.x=T)
names(wsd)[match(c("[10,30]", "(30,Inf]" ),names(wsd))] <- c("P10_30", "P30_plus")
wsd<-merge(wsd,r8,by=UNIQUE_SURVEY)
wsd<-merge(wsd,r9,by=UNIQUE_SURVEY)
wsd<-merge(wsd,r10,by=UNIQUE_SURVEY)

data.cols<-c(trophic.cols,"TotFish", "0_20", "20_50", "50_plus", "MEAN_SIZE", "P10_30", "P30_plus",fam.cols,ac.cols,sp.cols)

# check for NA values - THIS DOESN'T HELP ISOLATE RECORDS, JUST CHECKS FOR NA VALUES
a<-subset(wsd,is.na(wsd))
# remove NA values
test<- na.omit(wsd)
# compare site names in test and wsd
setdiff(test$SITE,wsd$SITE)
setdiff(wsd$SITE,test$SITE)
# subset sites: OAH-04452 NII-02798 KAU-00274 KAU-00281 KAU-00251
wtf<-wsd %>% filter(SITE == "OAH-04452"|SITE == "NII-02798"|SITE == "KAU-00274"|SITE == "KAU-00281"|SITE == "KAU-00251")

##### NA values in parrot sizes ####
wsd[is.na(wsd$P30_plus),]$P30_plus<-0
wsd[is.na(wsd$P10_30),]$P10_30<-0


# OUTPUT working_site_data (appendix 1) -----------------------------------
head(wsd)


# !!!!!!!!!!!!!!!!!!!!!!!!!! CHANGE FILE NAME EACH YEAR !!!!!!!!!!!!!!!!!!!!!!!!!
save(wsd, file="data/data outputs/working_site_data.rdata")

wsd.uncap<-wsd

# save uncapped data for maps
## DOING THIS ONLY WITH nSPC data ####
wsd.uncap<-subset(wsd.uncap, wsd.uncap$METHOD=="nSPC")
wsd.uncap<-droplevels(wsd.uncap)

## check which ISLANDS differ between sectors and working data..
setdiff(unique(sectors$ISLAND), unique(wsd.uncap$ISLAND))
setdiff(unique(wsd.uncap$ISLAND),unique(sectors$ISLAND)) # should be just Sarigan, Alamagan ad Guguan, fixed below

wsd.uncap<-droplevels(wsd.uncap)
# OUTPUT cleaned up working site data NOT CAPPED (appendix 8 and maps) --------------------------
# clean site level data, will do a bit more cleaning up for maps..
save(wsd.uncap, file="data/data outputs/clean_working_site_data_used_in_higher_pooling_for_report.Rdata")

### CAP DATA TO SOMEWHERE AROUND 97.5% PERCENTILE
for(i in 1:(length(data.cols)))
{
	cat(data.cols[i])
	cat(" ")
	cat(round(quantile(wsd[,data.cols[i]], c(0.9,0.95,0.975,.99), na.rm = T),1))
	cat("      ")
}
#
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  REALLY IMPORTANT THAT THESE CAPPING VALUES ARE SET APPROPRIATELY FOR EACH REPORT .. IE DO NOT JUsT USE THE SAME VALUES EACH ITERATION
#

wsd[wsd$PISCIVORE>420,]$PISCIVORE<-420
wsd[wsd$PLANKTIVORE>40,]$PLANKTIVORE<-40
wsd[wsd$PRIMARY>80,]$PRIMARY<-80
wsd[wsd$SECONDARY>43,]$SECONDARY<-43
wsd[wsd$TotFish>500,]$TotFish<-500
wsd[wsd$"0_20">45,]$"0_20"<-45
wsd[wsd$"20_50">106,]$"20_50"<-106
wsd[wsd$"50_plus">415,]$"50_plus"<-415
wsd[wsd$MEAN_SIZE>33,]$MEAN_SIZE<-33
wsd[wsd$P10_30>15,]$P10_30<-15
wsd[wsd$P30_plus>20,]$P30_plus<-20


# OUTPUT -----------------------------------
save(wsd, file="data/data outputs/working site data CAPPED.rdata")

write.csv(wsd,file="data/data outputs/working site data CAPPED.csv")
#load("data/data outputs/working site data CAPPED.rdata")
####################################################################################################################################################################
#
#     CHECK THAT DATA IS READY FOR POOLING AND DO SOME FINAL CLEAN UPS, EG SET BACKREEF DEPTH_ZONE TO ALL, CREATE THE "SGA" LOCATION
#
####################################################################################################################################################################

## check wwhether we have ISLANDS that arent in the sectors file
setdiff(unique(wsd$ISLAND),unique(sectors$ISLAND)) # may show alamagan, guguan, sarigan, fixed below

#set all Backreef to a single DEPTH_ZONE ("All")
levels(wsd$DEPTH_BIN)<-c(levels(wsd$DEPTH_BIN), "All")
wsd[wsd$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"
sectors[sectors$REEF_ZONE=="Backreef",]$DEPTH_BIN<-"All"

# wsd$DEPTH_BIN<-as.character(wsd$DEPTH_BIN)# won't change value to "All" if it is a factor
# wsd[wsd$ISLAND=="Rose" & wsd$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
# sectors[sectors$ISLAND=="Rose" & sectors$REEF_ZONE=="Lagoon",]$DEPTH_BIN<-"All"
# wsd$DEPTH_BIN<-as.factor(wsd$DEPTH_BIN)# change back to factor

wsd$STRATA<-paste(substring(wsd$REEF_ZONE,1,1), substring(wsd$DEPTH_BIN,1,1), sep="")
sectors$STRATA<-paste(substring(sectors$REEF_ZONE,1,1), substring(sectors$DEPTH_BIN,1,1), sep="")

# check for strata that don't have area from AREA_HA_2024
check<-sectors %>% filter(REGION=="MHI"|REGION=="NWHI") 
a<-subset(check,is.na(AREA_HA_2024))
a
# drop any sites from wsd that are MAI_MOLOKINI forereef shallow
b<-wsd %>% filter(SEC_NAME=="MAI_MOLOKINI"&DEPTH_BIN=="Shallow")

# drop sites from midway backreef or deep lagoon
b<-wsd %>% filter(ISLAND=="Midway"&REEF_ZONE=="Backreef")
l<-wsd %>% filter(ISLAND=="Midway"&REEF_ZONE=="Lagoon"&DEPTH_BIN=="Shallow")
#create a vector of site id's to remove
midremove<-c(b$SITEVISITID,l$SITEVISITID)
# filter from dataframe
wsd_filtered <- wsd %>%
  filter(!SITEVISITID %in% midremove)

wsd<-wsd_filtered

## generate a complete list of all ANALYSIS STRATA and their size
SCHEMES<-c("RAMP_BASIC", "MARI2011", "MARI2014", "TUT10_12", "AS_SANCTUARY")
##MODIFIED MODIFIED MODIFIED START
for(i in 1:length(SCHEMES)){
  tmp2<-sectors[,c("SEC_NAME",SCHEMES[i])]
  tmp2$SCHEME<-SCHEMES[i]
  names(tmp2)<- c("SEC_NAME", "ANALYSIS_SEC", "ANALYSIS_SCHEME")
  
  tmp<-aggregate(sectors$AREA_HA_2024, sectors[,c(SCHEMES[i], "STRATA")], sum)
  tmp$SCHEME<-SCHEMES[i]
  names(tmp)<-c("ANALYSIS_SEC", "STRATA", "AREA_HA_2024", "ANALYSIS_SCHEME")
  if(i==1){
    st<-tmp
    as<-tmp2
  } else {
    st<-rbind(st, tmp)
    as<-rbind(as, tmp2)
  }	
}
as$TMP<-1
as<-aggregate(as$TMP, by=as[,c("SEC_NAME", "ANALYSIS_SCHEME", "ANALYSIS_SEC")], length) 
as$x<-NULL

wsd<-merge(wsd, as, by=c("SEC_NAME", "ANALYSIS_SCHEME"), all.x=T)  # add ANALYSISS_SCHEME for tthis sector and sceheme combination
unique(wsd[is.na(wsd$ANALYSIS_SCHEME), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "STRATA")])

cast(st, ANALYSIS_SEC ~ ANALYSIS_SCHEME, value="AREA_HA_2024", sum)
wsd<-merge(wsd, st, by=c("ANALYSIS_SCHEME", "ANALYSIS_SEC", "STRATA"), all.x=T)
#check if some are missing an AREA_HA .. which means that they didnt get into the stratification scheme properly
unique(wsd[is.na(wsd$AREA_HA), c("ISLAND", "ANALYSIS_SEC", "SEC_NAME", "OBS_YEAR", "ANALYSIS_YEAR", "ANALYSIS_SCHEME", "STRATA")])
#### MODIFIED END

#NOW CHECK HOW MANY REPS WE HAVE PER STRATA
a<-cast(wsd, REGION + ANALYSIS_SCHEME + ISLAND + SEC_NAME + ANALYSIS_YEAR ~ STRATA, value="AREA_HA_2024", length); a

# OUTPUT sites per years (appendix 3) -------------------------------------
save(a, file="data/data outputs/sites_year_reef_zone_depth_bin.rdata") ## use this for table in appendix 3 - see appendices R file
write.csv(wsd,file="data/data outputs/check_wsd.csv")


####################################################################################################################################################################
#
#     POOL WSD (WORKING SITE DATA TO STRATA THEN TO HIGHER LEVELS
##
###################################################################################################################################################################

### CALCULATE MEAN AND VARIANCE WITHIN STRATA ###
SPATIAL_POOLING_BASE<-c("REGION", "ISLAND", "SEC_NAME", "REEF_ZONE", "STRATA")    
ADDITIONAL_POOLING_BY<-c("METHOD", "ANALYSIS_YEAR")                                    # additional fields that we want to break data at, but which do not relate to physical areas (eg survey year or method)

#generate within strata means and vars
POOLING_LEVEL<-c(SPATIAL_POOLING_BASE, ADDITIONAL_POOLING_BY)
dps<-Calc_PerStrata(wsd, data.cols, c(POOLING_LEVEL, "AREA_HA_2024"))
#save(dps,file="tmp REA per strata.RData")
head(dps$Mean)

###### REMOVE STRATA with N=1 (cannot pool those up)
dps$Mean<-dps$Mean[dps$Mean$N>1,]
dps$SampleVar<-dps$SampleVar[dps$SampleVar$N>1,]
dps$SampleSE<-dps$SampleSE[dps$SampleSE$N>1,]

# save by sector
OUTPUT_LEVEL<-c("REGION", "ISLAND", "SEC_NAME", "ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_2024")
save(dp, file="data/data outputs/MONREPdata_pooled_is_yr_SEC.Rdata")
write.csv(dp,file='data/data outputs/MONREPdata_pooled_is_yr_SEC.csv')

# e.g. SAVE BY ISLAND PER YEAR
OUTPUT_LEVEL<-c("REGION", "ISLAND", "REEF_ZONE", "ANALYSIS_YEAR") 
dp<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_2024")
save(dp, file="data/data outputs/MONREPdata_pooled_is_yr_RZ.Rdata")

# e.g. SAVE BY REGION PER YEAR
OUTPUT_LEVEL<-c("REGION", "ANALYSIS_YEAR") 
dpR<-Calc_Pooled_Simple(dps$Mean, dps$SampleVar, data.cols, OUTPUT_LEVEL, "AREA_HA_2024")
save(dpR, file="data/data outputs/MONREPdata_pooled_reg.rdata")


# GET ISLAND AND REGIONAL AVERAGES
## function to calculate pooled SE
pool_se<-function(se_vals, weights){
  df<-data.frame(se=se_vals, wt=weights)
  df<-df[!is.na(df$se),]
  if(dim(df)[1]==0) return(NaN)	
  
  weights<-df$wt/sum(df$wt)  #convert weights to portions
  tmp<-(df$se^2)*(weights^2)
  pooled.se<- sqrt(sum(tmp))
  return(pooled.se)
} #end pool_se

m<-dp$Mean; s<-dp$PooledSE
DATA_COLS<-c("PRIMARY", "SECONDARY", "PLANKTIVORE", "PISCIVORE", "TotFish", "0_20", "20_50", "50_plus", "MEAN_SIZE","P10_30","P30_plus",  "Carangidae","Carcharhinidae","Acanthuridae","CEAR","LUFU","LUKA","NEMA" )
MeanIs<-aggregate(m[,c(DATA_COLS, "TOT_AREA_WT")], by=m[,c("REGION", "ISLAND", "REEF_ZONE")], FUN=mean, na.rm = T); MeanIs
MeanIs$N<-0
SEIs<-MeanIs #create SE structure
for(i in 1:dim(SEIs)[1])
{
  base_d<-s[s$ISLAND==SEIs[i,]$ISLAND & s$REEF_ZONE==SEIs[i,]$REEF_ZONE,]
  SEIs[i,]$N<-MeanIs[i,]$N<-sum(base_d$N)
  SEIs[i, DATA_COLS]<-apply(base_d[,DATA_COLS],2, function(x) pool_se(x,rep(1,length(x))))
}

dpI<-list(MeanIs, SEIs)
names(dpI)<-list("Mean", "PooledSE")
save(dpI, file="data/data outputs/MONREPdata_pooled_is_RZ.rdata")

#Now get regional average for Forereef only
f_mean<-subset(dpI$Mean, dpI$Mean$REEF_ZONE =="Forereef" & dpI$Mean$ISLAND !="South Bank")
f_se<-subset(dpI$PooledSE, dpI$PooledSE$REEF_ZONE=="Forereef" & dpI$PooledSE$ISLAND !="South Bank")

reg_mean<-aggregate(f_mean[,DATA_COLS], by=f_mean[,c("REGION", "REEF_ZONE")], FUN=mean); reg_mean  #building the data structure
reg_mean$N<-0
reg_se<-reg_mean #make SE structure
for(i in 1:dim(reg_se)[1])
{
  base_mean<-f_mean[f_mean$REGION==reg_mean[i,]$REGION,]
  base_se<-f_se[f_se$REGION==reg_se[i,]$REGION,]
  reg_se[i,]$N<-reg_mean[i,]$N<-sum(base_mean$N)
  
  #weight by island forereef area
  reg_se[i, DATA_COLS]<-apply(base_se[,DATA_COLS],2, function(x) pool_se(x,base_se$TOT_AREA_WT)) 
  reg_mean[i,DATA_COLS]<-apply(base_mean[,DATA_COLS],2, function(x) weighted.mean(x, base_mean$TOT_AREA_WT)) 
}

dpR<-list(reg_mean, reg_se)
names(dpR)<-list("Mean", "PooledSE")
save(dpR, file="data/data outputs/MONREPdata_pooled_reg_FRF.rdata")

# save numbers for Regional comparisons section in text of report:U.S. Pacific reefs: the status of reef fishes _________FOREREEF ONLY!______
dp<-as.data.frame(dpR)
# get relevant columns
ndp<-dp[,c("Mean.REGION","Mean.REEF_ZONE","Mean.TotFish","PooledSE.TotFish")]
# sort by Mean.TotFish in descending order, round numbers to 1 decimal place
ndps<-ndp[order(-ndp$Mean.TotFish),]
ndps$Mean.TotFish<-round(ndps$Mean.TotFish,1)
ndps$PooledSE.TotFish<-round(ndps$PooledSE.TotFish,1)
write.csv(ndps,file="data/data outputs/Region_forereef_mean_totfish.csv")


