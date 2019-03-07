###################################################################
#
# Genetic Counseling Assessment of Mexico
#
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
#
# Date: Mar 5, 2019
#
###################################################################


rm(list=ls())


#----------------------------------
# 0. Add libraries
#----------------------------------


library(ggplot2)
library(ggrepel)
library(stringr)
library(ggpubr)
library(grDevices)

#if (!require("devtools")) {
#  install.packages("devtools")
#}
#devtools::install_github("diegovalle/mxmaps")

library("mxmaps")

#----------------------------------
# 1. Load data
#----------------------------------

data(mxstate.map)


states <- read.csv("infostates-mexico-large.csv")
states$id = str_pad(states$id, 2, pad = "0")

#----------------------------------
# 2. Plot data
#----------------------------------

regions = c("North" = "#4B5CAB", 
            "Central East" = "#D16B69",
            "Central West" = "#69BCE8",
            "South" = "#8CB838",
            "South East" = "#F2A03D")

regionLabs = c("North (N=2)", 
               "Central East (N=1)", 
               "Central West (N=4)", 
               "South (N=12)", 
               "South East (N=1)")


#state.colors <- unlist(lapply(states$region, function(x) regions[[as.character(x)]]))

#states <- aggregate(mxstate.map[,c("long", "lat")], by=list(mxstate.map$region), FUN=mean)


#Function to PLOT map
getMexicoStates <- function(data.map, states, colors, title){
  
  data.map$region <- unlist(lapply(data.map$region, function(x) states$region[which(states$id == x)]))
  
  
  gMap <- ggplot(data.map) +
    geom_polygon(aes(long, lat, fill=region, group=group), color = NA, size = 0.5) +
    geom_polygon(aes(long, lat, group=group), fill=NA, color = "white", size = 0.5) +
    #coord_map() +
    coord_fixed(1.3) +
    #geom_label(data=states, aes(long, lat, label=code), hjust = 0.5, color="black", 
    #           size=3, fontface="bold", alpha=0.5, fill="gray99") +
    geom_text(data=states, aes(long, lat, label=code), hjust = 0.5, color="black", 
              size=3, fontface="bold") +
    scale_fill_manual(breaks=names(colors), labels=regionLabs, values=colors, name="Region") +
    labs(subtitle = title, sep="") +
    theme_light() +
    theme(legend.position = c(0.2, 0.2),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          plot.subtitle = element_text(hjust=0.5, face="bold")
    )
  
  return(gMap)
}

normalize <- function(x, pop){
  y <- x/pop * 100000
  y <- (y-min(y))/(max(y)-min(y))
  return(y)
}

range01 <- function(x){(x-min(x))/(max(x)-min(x))}



#Participants by region
g1 <- getMexicoStates(mxstate.map, states, regions, "Participants")


ggsave(filename="figures/figure3-participants.eps", plot=g1, device=cairo_ps, 
       width = 10, height = 10, fallback_resolution = 600)

