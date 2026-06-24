### LOAD DATA #####
rm(list=ls())

## fish status report standard figures
## island section
library(reshape)
library(gdata)
library(grid)
library(gridExtra)
library(gridBase)
library(ggplot2)
library(dplyr)

## data for island graphs 

load("data/data outputs/MONREPdata_pooled_is_yr_RZ.rdata") ### island estimates mean over cruises since spc after 2009
rd<-as.data.frame(dp)
# change REGION, ISLAND, REEF_ZONE to character
dp$Mean.REGION<-as.character(dp$Mean.REGION)
dp$Mean.ISLAND<-as.character(dp$Mean.ISLAND)
dp$Mean.REEF_ZONE<-as.character(dp$Mean.REEF_ZONE)
dp$PooledSE.REGION<-as.character(dp$PooledSE.REGION)
dp$PooledSE.ISLAND<-as.character(dp$PooledSE.ISLAND)
dp$PooledSE.REEF_ZONE<-as.character(dp$PooledSE.REEF_ZONE)

# filter to only include data from 2010 onwards:

rd<-rd %>% filter(Mean.ANALYSIS_YEAR > "2008")
rd<-rd %>% filter(Mean.ISLAND != "South Bank")
write.csv(rd,file="data/data outputs/MONREPdata_pooled_is_yr_RZ.csv", row.names = F)
# save all data as rdd so can pull from that file for each region
rdd<-rd

## data for region reference lines - FOREREEF
load("data/data outputs/MONREPdata_pooled_reg_FRF.rdata")
ref<-as.data.frame(dpR) # forereef only reference

# change factors to character
str(ref)
ref$Mean.REGION<-as.character(ref$Mean.REGION)
ref$Mean.REEF_ZONE<-as.character(ref$Mean.REEF_ZONE)
ref$PooledSE.REGION<-as.character(ref$PooledSE.REGION)
ref$PooledSE.REEF_ZONE<-as.character(ref$PooledSE.REEF_ZONE)
write.csv(ref,file="data/data outputs/MONREPdata_pooled_reg_FRF.csv", row.names=F)
# NO ref lines for lagoon and backreef

# ## with the reference lines, need to run each region separately...
# # what areas:
# year<-rdd[rdd$Mean.ANALYSIS_YEAR == "2024",]
# year$Mean.ISLAND

# MHI -----------------------------------------------

## create reference lines for region
mhi_ref_cons<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.PRIMARY","Mean.SECONDARY","Mean.PLANKTIVORE","Mean.PISCIVORE")]))
# consumer groups

# for biomass of parrots in 2 size classes
mhi_ref_sc<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.P10_30","Mean.P30_plus")]))

# for  all fish biomass
mhi_ref_tb<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.TotFish")]))

# mean size
mhi_ref_sz<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI","Mean.MEAN_SIZE"]))

# size class
mhi_ref_szcl<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.20_50", "Mean.50_plus")]))

# species of interest
mhi_ref_sp<-as.numeric(as.vector(ref[ref$Mean.REGION=="MHI",c("Mean.Carangidae", "Mean.Carcharhinidae")]))

## references lines for plots, use ..._m (mean), sep (m plus se) and sem (m minus se)
hline.data.mhi_cons_m <- data.frame(z = mhi_ref_cons, Consumergroup = c("Primary", "Secondary","Planktivores","Piscivores")) ## needs to match the graph type

hline.data.mhi_sc_m <- data.frame(z = mhi_ref_sc, Size_class = c("Parrots 10-30 cm", "Parrots >30 cm")) ## needs to match the graph type

hline.data.mhi_tb_m <- data.frame(z = mhi_ref_tb, Size_class = "All fish") ## needs to match the graph type

hline.data.mhi_sz_m <- data.frame(z = mhi_ref_sz, mean_size = "All fish") ## needs to match the graph type

hline.data.mhi_ref_szcl<-data.frame(z = mhi_ref_szcl, Size = c("All fish 20-50 cm", "All fish >50 cm"))
hline.data.mhi_ref_sp<-data.frame(z =mhi_ref_sp, Species = c("Jacks", "Sharks"))

# save graphs in figures folder
setwd("Figures/MHI")

################ MHI FOREREEF #############
## with the reference lines, need to run each region separately...
rd<-rdd %>% filter(Mean.REGION == "MHI")
rd<-droplevels(rd)
rd<-rd %>% filter(Mean.REEF_ZONE == "Forereef")

## batch make the graphs 
for(i in 1:length(rd$Mean.ISLAND)){
  s<-rd$Mean.ISLAND[i]
  data<-subset(rd, Mean.ISLAND == s) ## change back to s
  #data<-subset(rd,Mean.ISLAND=="Tau");s<-"Tau"
  data<-drop.levels(data)
  
  ##########  parrot size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size_class", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size_class", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size_class", "Biomass", "SE")
  levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  group.colors<-c("#ebdef0", "#c39bd3")
  ## define labels so to have line breaks
  par(bg=NA)
  sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_fill_manual(values = group.colors)+
    geom_errorbar(limits, position=dodge, width=0.25)+
    facet_grid(~factor(Size_class, c("Parrots 10-30 cm", "Parrots >30 cm")),scales ="fixed") +
    scale_y_continuous(expand = c(0, 0, 0.05, 0))+
    geom_hline(aes(yintercept = z), hline.data.mhi_sc_m, linewidth = 1, colour = "black")+
    geom_hline(aes(yintercept = z), hline.data.mhi_sc_m, linewidth = .6, colour = "darkgrey")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 6))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_))+ # to avoid drawing plot outline)
    labs(title = "Parrotfish", y = expression(paste("Biomass (g ", m^-2,")")))
  
  ##########  all fish size class group graph
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "Mean.20_50", "Mean.50_plus")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Size", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "PooledSE.20_50", "PooledSE.50_plus")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Size", "Biomass", "SE")
  levels(dt$Size)<-c("All fish 20-50 cm", "All fish >50 cm")
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  group.colors<-c("#d98880", "#cd6155")
  ## define labels so to have line breaks
  
  sizeclassall <- ggplot(dt, aes(fill=Size, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_fill_manual(values = group.colors)+
    geom_errorbar(limits, position=dodge, width=0.25) +
    facet_grid(~factor(Size,c("All fish 20-50 cm", "All fish >50 cm")), scales ="fixed") +
    scale_y_continuous(expand = c(0, 0, 0.05, 0))+
    geom_hline(aes(yintercept = z), hline.data.mhi_ref_szcl, linewidth = 1, colour = "black")+
    geom_hline(aes(yintercept = z), hline.data.mhi_ref_szcl, linewidth = .6, colour = "darkgrey")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 6))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_blank())+
    theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_))+ # to avoid drawing plot outline)
    labs(y = expression(paste("Biomass (g ", m^-2,")")))
  
  ##########  Jacks and sharks #
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "Mean.Carangidae", "Mean.Carcharhinidae")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Family", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "PooledSE.Carangidae", "PooledSE.Carcharhinidae")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Family", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Species", "Biomass", "SE")
  levels(dt$Species)<-c("Jacks", "Sharks")
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  group.colors<-c("#9b59b6", "#76448a")
  ## define labels so to have line breaks
  
  priority <- ggplot(dt, aes(fill=Species, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    scale_fill_manual(values = group.colors)+
    geom_errorbar(limits, position=dodge, width=0.25) +
    facet_wrap(~Species, scales ="free") +
    scale_y_continuous(expand = c(0, 0, 0.05, 0))+
    geom_hline(aes(yintercept = z), hline.data.mhi_ref_sp, linewidth = 1, colour = "black")+
    geom_hline(aes(yintercept = z), hline.data.mhi_ref_sp, linewidth = .6, colour = "darkgrey")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 6))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_))+ # to avoid drawing plot outline)
    labs(title = "Families of Interest", y = expression(paste("Biomass (g ", m^-2,")")))
  
  ##########  mean size graph
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "Mean.MEAN_SIZE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_size", "Total_length")
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "PooledSE.MEAN_SIZE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_size", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_size", "Total_length", "SE")
  levels(dt$mean_size)<-"All fish"
  dt<-drop.levels(dt)
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE)
  dodge <- position_dodge(width=0.9)
  ## define labels so to have line breaks
  
  meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values="#c0392b") +
    facet_wrap(~mean_size)+
    scale_y_continuous(expand = c(0, 0, 0.05, 0))+
    geom_hline(aes(yintercept = z), hline.data.mhi_sz_m, linewidth = 1, colour = "black")+
    geom_hline(aes(yintercept = z), hline.data.mhi_sz_m, linewidth = .6, colour = "darkgrey")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 18))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_blank()) +
    theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_))+ # to avoid drawing plot outline)
    labs(y = expression(paste("Mean size (TL cm)")))
  
  ##########  total fish biomass graph
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "Mean.TotFish")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "mean_tot", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "PooledSE.TotFish")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "mean_tot", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","mean_tot", "Biomass", "SE")
  levels(dt$mean_tot)<-"All fish"
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  color<-"#e6b0aa"
  ## define labels so to have line breaks
  
  meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=color) +
    facet_wrap(~mean_tot)+
    scale_y_continuous(expand = c(0, 0, 0.05, 0))+
    geom_hline(aes(yintercept = z), hline.data.mhi_tb_m, linewidth = 1, colour = "black")+
    geom_hline(aes(yintercept = z), hline.data.mhi_tb_m, linewidth = .6, colour = "darkgrey")+
    theme_bw() + theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 35))+
    theme(axis.text.x = element_text(angle = 90, vjust = .5))+
    theme(plot.title = element_blank()) +
    theme(legend.position = "none")+
    theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_))+ # to avoid drawing plot outline)
    labs(y = expression(paste("Biomass (g ", m^-2,")")))
  
  
  ######total biomass per consumer group island per year
  
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
  test<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test)<-c("Island", "OBS_YEAR", "Consumergroup", "Biomass")
  tmr<-data[c("Mean.ISLAND","Mean.ANALYSIS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
  test2<-melt(tmr, id=c("Mean.ISLAND", "Mean.ANALYSIS_YEAR"))
  names(test2)<-c("Island","OBS_YEAR", "Consumergroup", "SE")
  
  dt<-as.data.frame(cbind(test, test2$SE))
  names(dt)<-c("Island", "Year","Consumergroup", "Biomass", "SE")
  levels(dt$Consumergroup)<-c("Primary", "Secondary", "Piscivores", "Planktivores")
  #dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
  
  ##Define the top and bottom of the errorbars
  limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
  dodge <- position_dodge(width=0.9)
  group.colors<-c("#ebf5fb", "#aed6f1","#2874a6","#5dade2")
  
  ## define labels so to have line breaks
  consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) +
    geom_bar(position="dodge", stat="identity", colour="black") +
    geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values = group.colors)+
    facet_grid(~factor(Consumergroup,c("Primary", "Secondary","Planktivores", "Piscivores")), scales ="fixed") +
    theme_bw() +
    scale_y_continuous(expand = c(0, 0, 0.05, 0))+
    geom_hline(aes(yintercept = z), hline.data.mhi_cons_m, linewidth = 1, colour = "black")+
    geom_hline(aes(yintercept = z), hline.data.mhi_cons_m, linewidth = .6, colour = "darkgrey")+
    theme(axis.title.x = element_blank()) +
    #coord_cartesian(ylim = c(0, 20))+
    theme(legend.position = "none")+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(panel.background = element_rect(fill = "transparent",
                                          colour = NA_character_), # to avoid drawing panel outline
          panel.grid.major = element_blank(), # get rid of major grid
          panel.grid.minor = element_blank(), # get rid of minor grid
          plot.background = element_rect(fill = "transparent",
                                         colour = NA_character_))+ # to avoid drawing plot outline)
    labs(title = "Consumer Group", y = expression(paste("Biomass (g ", m^-2,")")))
  
  png(filename = paste(s,"FRF_2024.png", sep = "_"), width = 6.5, height = 5, units = "in",  bg = "transparent", res = 600, restoreConsole = TRUE)

# grid.arrange(meanbio, meansize, sizeclassall,consgrp, sizeclass, priority, nrow = 3, ncol = 4,
#   top = "All fish"
# )   
 
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

  grid.newpage() #plot.new()
  pushViewport(viewport(layout = grid.layout(4, 4, heights = unit(c(0.5, 5,5,5),"null")))) # 4 rows (one small row for title), 4 columns
  print(sizeclassall, vp = vplayout(2, 3:4))  # this plot covers row 1 and cols 2:3
  print(meanbio,vp=vplayout(2,1))
  print(meansize, vp = vplayout(2, 2))
  print(consgrp, vp = vplayout(3, 1:4))
  print(sizeclass, vp = vplayout(4,1:2))
  print(priority, vp = vplayout(4,3:4))
  grid.text("      All Fish",gp = gpar(fontsize = 14),vp = vplayout(1,1:4))

  dev.off()
  
}

# # MHI SECTORS --------------------------------------
#  
# load("data/data outputs/MONREPdata_pooled_is_yr_SEC.Rdata")
# 
# rdd<-as.data.frame(dp)
# # change REGION, ISLAND to character
# rdd$Mean.REGION<-as.character(rdd$Mean.REGION)
# rdd$Mean.ISLAND<-as.character(rdd$Mean.ISLAND)
# rdd$PooledSE.REGION<-as.character(rdd$PooledSE.REGION)
# rdd$PooledSE.ISLAND<-as.character(rdd$PooledSE.ISLAND)
# 
# # save graphs in figures folder
# setwd("Figures/MHI")
# 
# rdd<-rdd %>% filter(Mean.REGION == "MHI")
# 
# ## batch make the graphs 
# for(i in 1:length(rdd$Mean.SEC_NAME)){
#   s<-rdd$Mean.SEC_NAME[i]
#   data<-subset(rdd, Mean.SEC_NAME == s) ## change back to s
#   #data<-subset(rd,Mean.SEC_NAME=="TUT_NE_OPEN")
#   data<-drop.levels(data)
#   
#   ##########  parrot size class group graph
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "Mean.P10_30", "Mean.P30_plus")]
#   test<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test)<-c("Sector", "OBS_YEAR", "Size_class", "Biomass")
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "PooledSE.P10_30", "PooledSE.P30_plus")]
#   test2<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test2)<-c("Sector","OBS_YEAR", "Size_class", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Sector", "Year","Size_class", "Biomass", "SE")
#   levels(dt$Size_class)<-c("Parrots 10-30 cm", "Parrots >30 cm")
#   
#   ##Define the top and bottom of the errorbars
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
#   dodge <- position_dodge(width=0.9)
#   group.colors<-c("#ebdef0", "#c39bd3")
#   ## define labels so to have line breaks
#   
#   sizeclass <- ggplot(dt, aes(fill=Size_class, y=Biomass, x=Year)) +
#     geom_bar(position="dodge", stat="identity", colour="black") +
#     scale_fill_manual(values = group.colors)+
#     geom_errorbar(limits, position=dodge, width=0.25)+
#     facet_grid(~factor(Size_class, c("Parrots 10-30 cm", "Parrots >30 cm")),scales ="fixed") +
#     # geom_hline(aes(yintercept = z), hline.data.MARS_sc_m, linewidth = 1, colour = "black")+
#     # geom_hline(aes(yintercept = z), hline.data.MARS_sc_m, linewidth = .6, colour = "darkgrey")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_text(hjust = 0.5))+
#     theme(panel.background = element_rect(fill = "transparent",
#                                           colour = NA_character_), # to avoid drawing panel outline
#           panel.grid.major = element_blank(), # get rid of major grid
#           panel.grid.minor = element_blank(), # get rid of minor grid
#           plot.background = element_rect(fill = "transparent",
#                                          colour = NA_character_))+ # to avoid drawing plot outline)
#     labs(title = "Parrotfish", y = expression(paste("Biomass (g ", m^-2,")")))
#   
#   ##########  all fish size class group graph
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "Mean.20_50", "Mean.50_plus")]
#   test<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test)<-c("Sector", "OBS_YEAR", "Size", "Biomass")
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "PooledSE.20_50", "PooledSE.50_plus")]
#   test2<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test2)<-c("Sector","OBS_YEAR", "Size", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Sector", "Year","Size", "Biomass", "SE")
#   levels(dt$Size)<-c("All fish 20-50 cm", "All fish >50 cm")
#   
#   ##Define the top and bottom of the errorbars
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
#   dodge <- position_dodge(width=0.9)
#   group.colors<-c("#d98880", "#cd6155")
#   ## define labels so to have line breaks
#   
#   sizeclassall <- ggplot(dt, aes(fill=Size, y=Biomass, x=Year)) +
#     geom_bar(position="dodge", stat="identity", colour="black") +
#     scale_fill_manual(values = group.colors)+
#     geom_errorbar(limits, position=dodge, width=0.25) +
#     facet_grid(~factor(Size,c("All fish 20-50 cm", "All fish >50 cm")), scales ="fixed") +
#     # geom_hline(aes(yintercept = z), hline.data.sam_ref_szcl, colour = "black")+
#     # geom_hline(aes(yintercept = z), hline.data.sam_ref_szcl, linewidth = .6, colour = "darkgrey")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_blank()) +
#     theme(panel.background = element_rect(fill = "transparent",
#                                           colour = NA_character_), # to avoid drawing panel outline
#           panel.grid.major = element_blank(), # get rid of major grid
#           panel.grid.minor = element_blank(), # get rid of minor grid
#           plot.background = element_rect(fill = "transparent",
#                                          colour = NA_character_))+ # to avoid drawing plot outline)
#     labs(y = expression(paste("Biomass (g ", m^-2,")")))
#   
#   ##########  jacks and sharks ###
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "Mean.Carangidae", "Mean.Carcharhinidae")]
#   test<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test)<-c("Sector", "OBS_YEAR", "Family", "Biomass")
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "PooledSE.Carangidae", "PooledSE.Carcharhinidae")]
#   test2<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test2)<-c("Sector","OBS_YEAR", "Family", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Sector", "Year","Family", "Biomass", "SE")
#   levels(dt$Family)<-c("Jacks", "Sharks")
#   
#   ##Define the top and bottom of the errorbars
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
#   dodge <- position_dodge(width=0.9)
#   group.colors<-c("#9b59b6", "#76448a")
#   ## define labels so to have line breaks
#   
#   priority <- ggplot(dt, aes(fill=Family, y=Biomass, x=Year)) +
#     geom_bar(position="dodge", stat="identity", colour="black") +
#     scale_fill_manual(values = group.colors)+
#     geom_errorbar(limits, position=dodge, width=0.25) +
#     facet_wrap(~Family, scales ="free") +
#     # geom_hline(aes(yintercept = z), hline.data.sam_ref_sp, colour = "black")+
#     # geom_hline(aes(yintercept = z), hline.data.sam_ref_sp, linewidth = .6, colour = "darkgrey")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_text(hjust = 0.5))+
#     theme(panel.background = element_rect(fill = "transparent",
#                                           colour = NA_character_), # to avoid drawing panel outline
#           panel.grid.major = element_blank(), # get rid of major grid
#           panel.grid.minor = element_blank(), # get rid of minor grid
#           plot.background = element_rect(fill = "transparent",
#                                          colour = NA_character_))+ # to avoid drawing plot outline)
#     labs(title = "Families of Interest", y = expression(paste("Biomass (g ", m^-2,")")))
#   
#   ##########  mean size graph
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "Mean.MEAN_SIZE")]
#   test<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test)<-c("Sector", "OBS_YEAR", "mean_size", "Total_length")
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "PooledSE.MEAN_SIZE")]
#   test2<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test2)<-c("Sector","OBS_YEAR", "mean_size", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Sector", "Year","mean_size", "Total_length", "SE")
#   levels(dt$mean_size)<-"All fish"
#   
#   ##Define the top and bottom of the errorbars
#   limits <- aes(ymax = Total_length + SE, ymin=Total_length - SE)
#   dodge <- position_dodge(width=0.9)
#   ## define labels so to have line breaks
#   meansize <- ggplot(dt, aes(fill=mean_size, y=Total_length, x=Year)) +
#     geom_bar(position="dodge", stat="identity", colour="black") +
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values="#c0392b") +
#     facet_wrap(~mean_size)+
#     # geom_hline(aes(yintercept = z), hline.data.MARS_sz_m, colour = "black")+
#     # geom_hline(aes(yintercept = z), hline.data.MARS_sz_m, linewidth = .6, colour = "darkgrey")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_blank()) +
#     theme(panel.background = element_rect(fill = "transparent",
#                                           colour = NA_character_), # to avoid drawing panel outline
#           panel.grid.major = element_blank(), # get rid of major grid
#           panel.grid.minor = element_blank(), # get rid of minor grid
#           plot.background = element_rect(fill = "transparent",
#                                          colour = NA_character_))+ # to avoid drawing plot outline)
#     labs(y = expression(paste("Mean size (TL cm)")))
#   
#   ##########  total fish biomass graph
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "Mean.TotFish")]
#   test<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test)<-c("Sector", "OBS_YEAR", "mean_tot", "Biomass")
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "PooledSE.TotFish")]
#   test2<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test2)<-c("Sector","OBS_YEAR", "mean_tot", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Sector", "Year","mean_tot", "Biomass", "SE")
#   levels(dt$mean_tot)<-"All fish"
#   
#   ##Define the top and bottom of the errorbars
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
#   dodge <- position_dodge(width=0.9)
#   color<-"#e6b0aa"
#   ## define labels so to have line breaks
#   
#   meanbio <- ggplot(dt, aes(fill=mean_tot, y=Biomass, x=Year)) +
#     geom_bar(position="dodge", stat="identity", colour="black") +
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values=color) +
#     facet_wrap(~mean_tot)+
#     # geom_hline(aes(yintercept = z), hline.data.MARS_tb_m, colour = "black")+
#     # geom_hline(aes(yintercept = z), hline.data.MARS_tb_m, linewidth = .6, colour = "darkgrey")+
#     theme_bw() + theme(axis.title.x = element_blank()) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0))+
#     theme(axis.text.x = element_text(angle = 90, vjust = .5))+
#     theme(plot.title = element_blank()) +
#     theme(legend.position = "none")+
#     theme(panel.background = element_rect(fill = "transparent",
#                                           colour = NA_character_), # to avoid drawing panel outline
#           panel.grid.major = element_blank(), # get rid of major grid
#           panel.grid.minor = element_blank(), # get rid of minor grid
#           plot.background = element_rect(fill = "transparent",
#                                          colour = NA_character_))+ # to avoid drawing plot outline)
#     labs(y = expression(paste("Biomass (g ", m^-2,")")))
#   
#   
#   ######total biomass per consumer group Sector per year
#   
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "Mean.PRIMARY", "Mean.SECONDARY", "Mean.PISCIVORE", "Mean.PLANKTIVORE")]
#   test<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test)<-c("Sector", "OBS_YEAR", "Consumergroup", "Biomass")
#   tmr<-data[c("Mean.SEC_NAME","Mean.ANALYSIS_YEAR", "PooledSE.PRIMARY", "PooledSE.SECONDARY", "PooledSE.PISCIVORE", "PooledSE.PLANKTIVORE")]
#   test2<-melt(tmr, id=c("Mean.SEC_NAME", "Mean.ANALYSIS_YEAR"))
#   names(test2)<-c("Sector","OBS_YEAR", "Consumergroup", "SE")
#   
#   dt<-as.data.frame(cbind(test, test2$SE))
#   names(dt)<-c("Sector", "Year","Consumergroup", "Biomass", "SE")
#   levels(dt$Consumergroup)<-c("Primary", "Secondary", "Piscivores", "Planktivores")
#   #dt$Consumergroup<-factor(dt$Consumergroup, levels(dt$Consumergroup)[c(4,1,2,3)])
#   
#   ##Define the top and bottom of the errorbars
#   limits <- aes(ymax = Biomass + SE, ymin=Biomass - SE)
#   dodge <- position_dodge(width=0.9)
#   group.colors<-c("#ebf5fb", "#aed6f1","#2874a6","#5dade2")
#   
#   ## define labels so to have line breaks
#   consgrp <- ggplot(dt, aes(fill=Consumergroup, y=Biomass, x=Year)) +
#     geom_bar(position="dodge", stat="identity", colour="black") +
#     geom_errorbar(limits, position=dodge, width=0.25) + scale_fill_manual(values = group.colors)+
#     facet_grid(~factor(Consumergroup,c("Primary", "Secondary","Planktivores", "Piscivores")), scales ="fixed") +
#     theme_bw() +
#     # geom_hline(aes(yintercept = z), hline.data.MARS_cons_m, colour = "black")+
#     # geom_hline(aes(yintercept = z), hline.data.MARS_cons_m, linewidth = .6, colour = "darkgrey")+
#     theme(axis.title.x = element_blank()) +
#     scale_y_continuous(expand = c(0, 0, 0.05, 0))+
#     theme(legend.position = "none")+
#     theme(axis.text.x = element_text(angle = 90, vjust = 0.5))+
#     theme(plot.title = element_text(hjust = 0.5))+
#     theme(panel.background = element_rect(fill = "transparent",
#                                           colour = NA_character_), # to avoid drawing panel outline
#           panel.grid.major = element_blank(), # get rid of major grid
#           panel.grid.minor = element_blank(), # get rid of minor grid
#           plot.background = element_rect(fill = "transparent",
#                                          colour = NA_character_))+ # to avoid drawing plot outline)
#     labs(title = "Consumer group", y = expression(paste("Biomass (g ", m^-2,")")))
#   
#   
#   png(filename = paste(s,"SEC_2024.png", sep = "_"), width = 6.5, height = 5, units = "in",  bg = "transparent", res = 600, restoreConsole = TRUE)
#   
#   vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
#   
#   grid.newpage() #plot.new()
#   pushViewport(viewport(layout = grid.layout(4, 4, heights = unit(c(0.5, 5,5,5),"null")))) # 4 rows (one small row for title), 4 columns
#   print(sizeclassall, vp = vplayout(2, 3:4))  # this plot covers row 1 and cols 2:3
#   print(meanbio,vp=vplayout(2,1))
#   print(meansize, vp = vplayout(2, 2))
#   print(consgrp, vp = vplayout(3, 1:4))
#   print(sizeclass, vp = vplayout(4,1:2))
#   print(priority, vp = vplayout(4,3:4))
#   grid.text("      All Fish",gp = gpar(fontsize = 14),vp = vplayout(1,1:4))
#   
#   dev.off()
#   
# }
# 
