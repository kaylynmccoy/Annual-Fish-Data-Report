# Maps of site level data for monitoring report - Hawaii

# set up workspace -----------------------
library(tidyverse)
library(ggplot2)
library(scales) #for muted colors in ggplot2
library(RColorBrewer) # for color palettes in ggplot2
library(sf)
library(ggnewscale)
library(ggspatial)
library(ggtext) # to change legend text
library(raster) # to extract island extents
library(ggrepel) # to repel labels on plots


# LOAD FISH DATA
load("data/data outputs/working site data CAPPED.rdata") # loads as 'wsd'

# Get relevant data
head(wsd)
wsd<-wsd %>% filter(OBS_YEAR>2008)
wsd<-wsd %>% filter(METHOD == "nSPC")
wsd<-droplevels(wsd)

# calculate how many surveys for each year
wsd<-wsd %>% mutate(n=1)

# read island shp file
island <- st_read("shapefiles/islands.shp",stringsAsFactors = F)

# read MHI sector shapefile
mhi_sec<- st_read("shapefiles/MHI_Sectors_2019.shp", stringsAsFactors = F)
# change SEC_NAME to Sector
mhi_sec<-mhi_sec %>% dplyr::rename(Sector = SEC_NAME)

# transform tut_sec to WGS84 (EPSG:4326) to match 'island'
mhi_sec <- st_transform(mhi_sec, crs = st_crs(island))

# check to confirm it's now in decimal degrees
st_bbox(mhi_sec)

# # read NMSAS - sanctuaries shapefile
# sanc<-st_read("shapefiles/NMSAS_PY.shp",stringsAsFactors = F)

# # assign the correct projected CRS 
# st_crs(sanc) <- 32702  # UTM Zone 2S with WGS84 datum
# 
# # transform sanc to WGS84 (EPSG:4326) to match 'island'
# sanc <- st_transform(sanc, crs = st_crs(island))
# 
# # check to confirm it's now in decimal degrees
# st_bbox(sanc)

# get bounding limits for each island
ext=read.csv("data/island_extents_for_cruisebrief_maps.csv")

#Extract the extent data from an IslandCode as a object of class EXTENT
IC2ext=function(ext,IC){
  i=which(ext$ISLANDCODE==IC)
  return(extent(ext$LEFT_XMIN[i],ext$RIGHT_XMAX[i],ext$BOTTOM_YMIN[i],ext$TOP_YMAX[i]))
}

#e.g.
 IC2ext(ext,"NII")
# IC2ext(ext,"TUT")
# IC2ext(ext,"SWA")
# IC2ext(ext,"TAU")
# IC2ext(ext,"ROS")

### NIIHAU ####
 is_extent<-IC2ext(ext,"NII")
 extent_df<-data.frame(
   xmin = is_extent@xmin,
   xmax = is_extent@xmax,
   ymin = is_extent@ymin,
   ymax = is_extent@ymax
 )
 # subset wsd for NII
 wsdi<-wsd %>% filter(ISLAND == "Niihau")
 # subset sectors for niihau
 seci<-mhi_sec %>% filter(ISL_CODE == "NII")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_discrete(palette = "Pastel1")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                        colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/NII_2024.png", width=10, height = 5, units = c("in")))
p



### KAUAI ####
#xlim = c(-159.8987 , -159.1813 ), ylim=c( 21.76507 ,22.34379 )
is_extent<-IC2ext(ext,"KAU")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)
# subset wsd for KAI
wsdi<-wsd %>% filter(ISLAND == "Kauai")
# subset sectors for niihau
seci<-mhi_sec %>% filter(ISL_CODE == "KAU")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_discrete(palette = "Pastel1")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/KAU_2024.png", width=10, height = 5, units = c("in")))
p


### OAHU ####
# xlim = c(-158.4007 , -157.5177 ), ylim=c( 21.14674 ,21.81164 )
is_extent<-IC2ext(ext,"OAH")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)
wsdi<-wsd %>% filter(ISLAND == "Oahu")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2011")
# subset sectors for niihau
seci<-mhi_sec %>% filter(ISL_CODE == "OAH") %>% filter(Sector != "OAH_PEARL")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_discrete(palette = "Pastel1")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/OAH_2024.png", width=10, height = 5, units = c("in")))
p

### molokai ####
is_extent<-IC2ext(ext,"MOL")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)
# MOL	Molokai	xlim = c(	-156.65,	-157.39), ylim=c(20.98,	21.29)
wsdi<-wsd %>% filter(ISLAND == "Molokai")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2011")
# subset sectors for ISLAND
seci<-mhi_sec %>% filter(ISL_CODE == "MOL")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_discrete(palette = "Pastel1")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/MOL_2024.png", width=10, height = 5, units = c("in")))
p

### lanai ####

# Lanai	xlim = c(-156.7498,	-157.115), ylim=c(20.968,	20.695)
is_extent<-IC2ext(ext,"LAN")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)
wsdi<-wsd %>% filter(ISLAND == "Lanai")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2011")
# subset sectors for ISLAND
seci<-mhi_sec %>% filter(ISL_CODE == "LAN")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_discrete(palette = "Pastel1")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/LAN_2024.png", width=10, height = 5, units = c("in")))
p

### Maui ####
# xlim = c(-155.927,	-156.774), ylim=c(21.0612,	20.501)
is_extent<-IC2ext(ext,"MAI")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

wsdi<-wsd %>% filter(ISLAND == "Maui")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2011")
# subset sectors for ISLAND
seci<-mhi_sec %>% filter(ISL_CODE == "MAI")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_discrete(palette = "Pastel1")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/MAI_2024.png", width=10, height = 5, units = c("in")))
p

### Kahoolawe ####
# Kahoolawe	20.645,	20.48	-156.514,	-156.733
is_extent<-IC2ext(ext,"KAH")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

wsdi<-wsd %>% filter(ISLAND == "Kahoolawe")
# remove year 2010for better viewing
#wsdi<-wsdi %>% filter(OBS_YEAR>"2011")
# subset sectors for ISLAND
seci<-mhi_sec %>% filter(ISL_CODE == "KAH")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_discrete(palette = "Pastel1")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/KAH_2024.png", width=10, height = 5, units = c("in")))
p



# hawaii ####
#xlim = c(-156.1712,-154.6997 ), ylim=c(18.60355,20.37775)
is_extent<-IC2ext(ext,"HAW")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

wsdi<-wsd %>% filter(ISLAND == "Hawaii")
# remove year 2010for better viewing
#wsdi<-wsdi %>% filter(OBS_YEAR>"2011")
# subset sectors for ISLAND
seci<-mhi_sec %>% filter(ISL_CODE == "HAW")
par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  geom_sf(data = seci, color = "black", aes(fill = Sector),alpha = 0.5) +
  scale_fill_brewer(palette = "Dark2")+
  #geom_sf(data = island, aes(fill = "")) +
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.box="vertical",legend.margin=margin(),legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+#guides(fill=guide_legend(nrow=2))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/MHI/HAW_2024.png", width=10, height = 5, units = c("in")))
p


#### Papahanoumokuakea ############################
# which islands/atolls?
n<-wsd %>% filter(REGION == "NWHI")
unique(n$ISLAND)
#French Frigate, Pearl & Hermes, Kure,Lisianski,Laysan,Midway,Necker ,Maro,Nihoa,Gardner    

### FFS/LALO #####
# xmin       : -166.4736,-165.9456 
# ymin       : 23.52903,23.97923
is_extent<-IC2ext(ext,"FFS")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

wsdi<-wsd %>% filter(ISLAND == "French Frigate")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2011")

par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/NWHI/FFS_2024.png", width=10, height = 5, units = c("in")))
p

### Midway #####
# xmin       :-176.1239 , -175.6099 
# ymin       :  27.65731, 28.06404 
is_extent<-IC2ext(ext,"MID")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

wsdi<-wsd %>% filter(ISLAND == "Midway")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2010")

par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/NWHI/MID_2024.png", width=10, height = 5, units = c("in")))
p

### Kure #####
# xmin       : -178.40 , -178.26 
# ymin       : 28.35, 28.48
is_extent<-IC2ext(ext,"KUR")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

wsdi<-wsd %>% filter(ISLAND == "Kure")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2009")

par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/NWHI/KUR_2024.png", width=10, height = 5, units = c("in")))
p

### lISI #####
# xmin       : -174.2028,-173.7052 
# ymin       : 25.7812 , 26.20444 
is_extent<-IC2ext(ext,"LIS")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

wsdi<-wsd %>% filter(ISLAND == "Lisianski")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2011")

par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/NWHI/LIS_2024.png", width=10, height = 5, units = c("in")))
p

### LAYSAN #####
is_extent<-IC2ext(ext,"LAY")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

# # # TO ADJUST MANUALLY: 
# extent_df$xmin = -171.78
# extent_df$xmax = -171.67
# extent_df$ymin = 25.71
# extent_df$ymax = 25.81

wsdi<-wsd %>% filter(ISLAND == "Laysan")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2011")

par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/NWHI/LAY_2024.png", width=10, height = 5, units = c("in")))
p

### PHR #####
is_extent<-IC2ext(ext,"PHR")
extent_df<-data.frame(
  xmin = is_extent@xmin,
  xmax = is_extent@xmax,
  ymin = is_extent@ymin,
  ymax = is_extent@ymax
)

# # # TO ADJUST MANUALLY:
 extent_df$xmin = -176.05
 extent_df$xmax = -175.69
 extent_df$ymin = 27.7
 extent_df$ymax = 28.0

wsdi<-wsd %>% filter(ISLAND == "Pearl & Hermes")
# remove year 2010for better viewing
wsdi<-wsdi %>% filter(OBS_YEAR>"2010")

par(bg=NA)
p<-ggplot(data=island)+
  geom_sf(fill="antiquewhite")+theme_bw()+theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),panel.background = element_rect(fill = "#d6eaf8"),axis.title.x = element_blank(), axis.title.y = element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank(), axis.ticks.y=element_blank(),)+
  annotation_scale(location = "bl", bar_cols = "black",width_hint = 0.15, text_cex = .9)+ # width_hint = proportion of panel to extend 
  coord_sf(xlim = c(extent_df$xmin,extent_df$xmax ), ylim=c(extent_df$ymin,extent_df$ymax), expand = FALSE)+
  geom_point(data = wsdi, mapping = aes(x = LONGITUDE, y = LATITUDE, size = TotFish), fill = "#f1948a",pch = 21)+
  scale_size_area(breaks = c(25, 50, 75,100,150))+
  facet_wrap(. ~ OBS_YEAR)+
  labs(size = paste("Biomass (g m<sup>-2</sup>)"))+
  theme(strip.text = element_text(size = 13),legend.title = element_markdown(size = 14), legend.position = "bottom", legend.key = element_rect(fill = "transparent", color = NA),legend.text = element_text(size = 13))+
  theme(plot.background = element_rect(fill = "transparent",
                                       colour = NA_character_))
suppressMessages(ggsave(filename ="figures/NWHI/PHR_2024.png", width=10, height = 5, units = c("in")))
p
