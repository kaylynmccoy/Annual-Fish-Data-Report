#################################################################################
### Format Papahānaumokuākea Marine National Monument Program data to match NCRMP
#################################################################################


#!!RUN THIS OUTSIDE OF THE PROJECT SO YOU CAN CONNECT TO THE NETWORK DRIVE!
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(maps)
library(sp)
library(sf)
library(stringr)

# bring in PMNM collected data - get file from brian.hauk@noaa.gov or chelsie.counsell@noaa.gov
mon<-read.csv("data/PMNM_RAMP2024_Report_ Fish REA Base.csv")

# take out niihau and add REGION, SEC_NAME, EXCLUDE_FLAG, HABITAT TYPE
mon<-mon %>% filter(ISLAND != "Niihau") %>% mutate(REGION = REGION_NAME, SEC_NAME = ISLAND, EXCLUDE_FLAG ="0", HABITAT_TYPE = HABITAT_CODE,PROGRAM = "PMNM")

# # bring in NCRMP data to match field names
# setdiff( names(ncrmp), names(mon))

# subset to columns we use 
mo<-mon %>% select(SITEVISITID, METHOD, DATE_, OBS_YEAR, SITE,  ISLAND, LATITUDE,  LONGITUDE,  REGION, REGION_NAME, SEC_NAME,  EXCLUDE_FLAG, TRAINING_YN,REP, REPLICATEID, DIVER, HABITAT_CODE, HABITAT_TYPE, DEPTH, SPECIES, COUNT, SIZE_, OBS_TYPE, PROGRAM)

# fill in depth bin with depth info
mo<-mo %>% mutate(DEPTH_BIN = ifelse(DEPTH < 6, "Shallow", ifelse(DEPTH > 18, "Deep", "Mid")))

# remove sites that are deeper than 30m = CCR
#test<-mo %>% filter(DEPTH>30);summary(test)
mo<-mo %>% filter(DEPTH<30)

# change spaces to underscores
mo$ISLAND = gsub(" ", "_", mo$ISLAND)

ref = read.csv("data/island_name_code.csv") %>%
  mutate(GIS_reference_reefzone = na_if(GIS_reference_reefzone, ""))

### get reef zone from shapefiles  ------------------------ 
# !!!!!! change file path to access GIS reefzone files (shp_dir)
# create function to convert to utm
get_utm <- function(x, y, zone, loc){
  points = SpatialPoints(cbind(x, y), proj4string = CRS("+proj=longlat +datum=WGS84"))
  points_utm = spTransform(points, CRS(paste0("+proj=utm +zone=", zone[1]," +ellps=WGS84 +north")))
  if (loc == "x") {
    return(coordinates(points_utm)[,1])
  } else if (loc == "y") {
    return(coordinates(points_utm)[,2])
  }
}

# add placeholder in dataframe
df_rz = NULL

for (isl in 1:length(unique(mo$ISLAND))) {
  
  # isl = 5
  
  shp_dir <- "Z:/GIS/Projects/CommonMaps/Reefzone"
  
  df_i = mo %>% subset(ISLAND == unique(mo$ISLAND)[isl])
  df_i <- df_i %>% select(-any_of("REEF_ZONE"))
  setnames = names(df_i)
  
  ref_i = ref %>% subset(Island == unique(mo$ISLAND)[isl])
  reef_code <- ref_i$GIS_reference_reefzone  # e.g., "kur"
  
  # Check and read
  if (is.na(reef_code) == T) {
    
    cat("No shapefile found matching ", reef_code)
    
    # if sites do not fall into a sector, create island name as the sector name
    df_i$REEF_ZONE = "Forereef"
    
    df_i <- df_i[, c(setnames, "REEF_ZONE")]
    
    df_rz = rbind(df_rz, df_i)
    next
  }
  
  # Find shapefile matching the reef code in its name
  shp_file <- list.files(
    path = shp_dir,
    pattern = paste0(reef_code, ".*//.shp$"),  # regex: reef code followed by anything, ending in .shp
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  shp <- st_read(shp_file[1], quiet = TRUE) %>% as("Spatial")
  
  # re-project in correct utm zone 
  zone <- (floor((df_i$LONGITUDE[1] + 180)/6) %% 60) + 1
  
  xy_utm = data.frame(x = df_i$LONGITUDE, y = df_i$LATITUDE)  %>% 
    mutate(zone2 = (floor((x + 180)/6) %% 60) + 1, keep = "all") %>% 
    mutate(zone2 = ifelse(unique(mo$ISLAND)[isl] == "Hawaii", 4, zone2)) %>%  # Add this line for the conditional statement
    group_by(zone2) %>% 
    mutate(utm_x = get_utm(x, y, zone2, loc = "x"),
           utm_y = get_utm(x, y, zone2, loc = "y")) %>% 
    ungroup() %>% 
    select(utm_x, utm_y) %>% 
    as.data.frame()
  
  colnames(xy_utm) = c("X", "Y")
  df_i = cbind(df_i, xy_utm)
  
  # create a spatial object
  latlon = df_i[,c("X", "Y")]
  coordinates(latlon) = ~X+Y
  
  if (median(df_i$LATITUDE) > 0) CRS.new <- CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +north"))
  if (median(df_i$LATITUDE) < 0) CRS.new <- CRS(paste0("+proj=utm +zone=", zone, " +datum=WGS84 +units=m +no_defs +south"))
  
  proj4string(latlon) <- CRS.new
  shp <- spTransform(shp, CRS.new)
  proj4string(shp) <- CRS.new # WARNING MESSAGE OK
  area <- over(latlon, shp)
  
  #plot(shp); axis(1); axis(2)
  #points(latlon, col = 2, pch = 20)
  
  zone_colname <- intersect(c("REEF_ZONE", "Zone", "Zones"), names(area))[1]
  
  zone_vals <- area %>%
    select(all_of(zone_colname)) %>%
    dplyr::rename(REEF_ZONE = all_of(zone_colname))
  
  df_i <- bind_cols(df_i, zone_vals)
  
  df_i <- df_i[, c(setnames, "REEF_ZONE")]
  
  df_rz = rbind(df_rz, df_i)
  
}
# plot to check
df_rz %>% 
  ggplot(aes(LONGITUDE, LATITUDE, fill = REEF_ZONE)) + 
  geom_point(shape = 21, size = 5, alpha = 0.8) + 
  scale_fill_viridis_d("") + 
  facet_wrap(~ISLAND, scales = "free")

# change island names back to NCRMP naming convention
df_rz$ISLAND = gsub("_"," ", df_rz$ISLAND)
# Change: Outer Back Reef & Inner Back Reef & Back Reef to Backreef, NA = Forereef, Inner Lagoon = Lagoon
df_rz$REEF_ZONE<- dplyr::recode(df_rz$REEF_ZONE, "Back Reef"="Backreef", "Inner Back Reef"="Backreef", "Outer Back Reef"= "Backreef","Inner Lagoon" = "Lagoon")
# replace NA values with forereef
df_rz <- df_rz %>% 
  mutate(REEF_ZONE = replace_na(REEF_ZONE, "Forereef"))
unique(df_rz$REEF_ZONE)

# merge with species list
# load species info --> !!! Important to use this list - includes a lot of clean up TLK used to do later in the code (that code has been removed)
spp.list<-read.csv("data/NCRMP Fish Species List - CLEAN.csv", header=T, na.strings=c("","NA")) %>%
  mutate_at(vars(LW_A, LW_B, LMAX, LENGTH_CONVERSION_FACTOR, SLTLRAT, FLTLRAT), ~as.numeric(.)) 
# errors ok, creates NA values for NONFISH entries

# ADD TAXA INFO
x<-df_rz
# PSPU and PSAT are a fish complex --> historically, we used PSAT, and more recently included PSPU in addition; COMBINE & CALL ALL OF THEM PSATs AS A COMPLEX!
x <- x %>%  mutate_at(vars(SPECIES), ~as.character(.)) %>% mutate(SPECIES = ifelse(SPECIES == "PSPU", "PSAT", SPECIES)) %>% 
  # add cleaned taxa info
  left_join(spp.list %>% select(-OLDTAXONNAME, -COMMONFAMILY, -LENTYP, -SLTLRAT, -FLTLRAT, -SOURCE, -CRYPTIC), by = c("SPECIES")) #%>% filter(is.na(SCIENTIFICNAME)) %>% distinct(SPECIES, FAMILY)

# UPDATE SITE NAMES SO CONSISTENT # OF CHARACTERS --> 9 characters based on Ivor's code
x <- x %>% mutate_at(vars(SITE), ~as.character(.)) %>% 
  separate(SITE, c("ISL", "NUM"), sep = "-", remove = FALSE) %>% #distinct(nchar(ISL), nchar(NUM)) --> ISL = 3, NUM = 1-4
  mutate(NUM.fixed = ifelse(nchar(NUM) < 5, str_pad(NUM, 5, side = "left", pad = "0"), NUM)) %>% # pad all #s to reach 6 digits
  select(-SITE) %>%
  unite("SITE", c("ISL", "NUM.fixed"), sep = "-", remove = TRUE) %>%
  select(-NUM)
# ADD ANALYSIS YEAR AND ANALSIS SCHEME FIELDS

x$ANALYSIS_YEAR<-"2024"
x$ANALYSIS_SCHEME <-"RAMP_BASIC"

x <- x %>% select(SITEVISITID, METHOD, DATE_, OBS_YEAR,  SITE, REEF_ZONE,  DEPTH_BIN,  ISLAND, LATITUDE,  LONGITUDE,  REGION , REGION_NAME,  EXCLUDE_FLAG, TRAINING_YN,
                  REP, REPLICATEID, DIVER, HABITAT_CODE, DEPTH, SPECIES, COUNT, SIZE_, OBS_TYPE, SCIENTIFIC_NAME,  TAXONNAME, COMMONNAME, GENUS,FAMILY, COMMONFAMILYALL, LMAX, LW_A,  LW_B,  LENGTH_CONVERSION_FACTOR, TROPHIC, TROPHIC_MONREP,SEC_NAME,ANALYSIS_YEAR,ANALYSIS_SCHEME, PROGRAM)

# # match date format - change to posixct object, specify input format, then change the format (may not need to be changed, might do it automatically to %Y-%m-%d)
x$DATE_ <- as.POSIXct(x$DATE_, format="%d-%b-%y")
str(x$DATE_)
#x$DATE_<-format(x$DATE_, "%Y-%m-%d")

# save clean data file
PMNM_DATA_FORMATTED<-x
save(PMNM_DATA_FORMATTED, file = "data/data outputs/PMNM_DATA_FORMATTED.Rdata")

