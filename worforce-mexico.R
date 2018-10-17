###################################################################
#
# Genetic Counseling Assessment of Mexico
#
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
#
# Date: Oct 7, 2018
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

#if (!require("devtools")) {
#  install.packages("devtools")
#}
#devtools::install_github("diegovalle/mxmaps")

library("mxmaps")

#----------------------------------
# 1. Load data
#----------------------------------

data(mxstate.map)


states <- read.csv("infostates-mexico.csv")
states$id = str_pad(states$id, 2, pad = "0")

#----------------------------------
# 2. Plot data
#----------------------------------

regions = c("North" = "#4B5CAB", 
            "Central East" = "#D16B69",
            "Central West" = "#69BCE8",
            "South" = "#8CB838",
            "South East" = "#F2A03D")


#state.colors <- unlist(lapply(states$region, function(x) regions[[as.character(x)]]))

#states <- aggregate(mxstate.map[,c("long", "lat")], by=list(mxstate.map$region), FUN=mean)


#Function to PLOT map
getMexicoStates <- function(data.map, states, colors, alphas, title){
  
  data.map$region <- unlist(lapply(data.map$region, function(x) states$region[which(states$id == x)]))
  
  
  gMap <- ggplot(data.map) +
    geom_polygon(aes(long, lat, group=group), fill=NA, color = "gray90", size = 0.5) +
    geom_polygon(aes(long, lat, fill=region, alpha=id, group=group), color = NA, size = 0.5) +
    #coord_map() +
    coord_fixed(1.3) +
    geom_text(data=states, aes(long, lat, label=code), hjust = 0.5, color="gray40", size=1.5) +
    scale_fill_manual(values=colors, name="Region") +
    scale_alpha_manual(values=alphas, guide ="none") +
    labs(subtitle = title, sep="") +
    theme_light() +
    theme(legend.position = "bottom",
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


getStatesBar <- function(mydata, title, ylab, color=c("slategray3", "thistle3")){
  
  mydata$newY <- scale(mydata$y, center = TRUE, scale = FALSE)
  mydata$col <- unlist(lapply(mydata$newY, function(y) if(y>0){"Above"}else{"Below"}))
  
  avg=attr(mydata$newY,"scaled:center")
  
  gBar <- ggplot(mydata) +
    geom_segment(aes(x=reorder(x, y), xend=reorder(x, y), y=avg, yend=y), size=1, color="gray66") +
    geom_hline(yintercept=avg, color="gray", linetype="dashed") +
    geom_point(aes(x=reorder(x, y), y=y, color=col), size=4.5) +
    geom_text(aes(x=reorder(x, y), y=y, label=round(y, digits=1)), size=2) +
    coord_flip() +
    scale_color_manual(breaks=mydata$col, values=color, name="Average") +
    labs(subtitle=title, y=ylab) +
    theme_light() +
    theme(legend.position = "bottom",
          #aspect.ratio=1.5,
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          axis.text = element_text(size=7),
          axis.title.x = element_text(size=9),
          axis.title.y = element_blank(),
          plot.subtitle = element_text(hjust=0.5, face="bold")
          )
  
  return(gBar)
  
}

#Medical Programs
#programs.alpha <- normalize(states$medical.programs, states$population)
programs.alpha <- range01(states$medical.programs)
names(programs.alpha) = states$id
g1 <- getMexicoStates(mxstate.map, states, regions, programs.alpha, "Medical Programs")

programs = data.frame(x=states$name, y=states$medical.programs)
#programs = rbind(programs, data.frame(x="National", y=mean(states$medical.programs), color="firebrick2"))
g5 <- getStatesBar(programs, "Medical Programs", "Number of Medical Programs", c("slategray3", "thistle3"))

#Medical Students
students.alpha <- range01(states$medical.students)
names(students.alpha) = states$id
g2 <- getMexicoStates(mxstate.map, states, regions, students.alpha, "Medical Students")

students = data.frame(x=states$name, y=states$medical.students)
#students = rbind(students, data.frame(x="National", y=mean(states$medical.students/states$population*100000), color="firebrick2"))
g6 <- getStatesBar(students, "Medical Students", "Number of Medical Students", c("slategray3", "thistle3"))



#Medical Professionals
professionals.alpha <- normalize(states$medical.professionals, states$population)
names(professionals.alpha) = states$id
g3 <- getMexicoStates(mxstate.map, states, regions, professionals.alpha, "General Practitioners")

professionals = data.frame(x=states$name, y=states$medical.professionals/states$population*100000, color=rep("gray55", 32))
#professionals = rbind(professionals, data.frame(x="National", y=mean(states$medical.professionals/states$population*100000), color="firebrick2"))
g7 <- getStatesBar(professionals, "General Practitioners", "Number of General Practitioners\nper 100,000 population", c("slategray3", "thistle3"))


#Medical Geneticists
geneticists.alpha <- normalize(states$medical.geneticists, states$population)
names(geneticists.alpha) = states$id
g4 <- getMexicoStates(mxstate.map, states, regions, geneticists.alpha, "Medical Geneticists")


geneticists = data.frame(x=states$name, y=states$medical.geneticists/states$population*100000, color=rep("gray55", 32))
#geneticists = rbind(geneticists, data.frame(x="National", y=mean(states$medical.geneticists/states$population*100000), color="firebrick2"))
g8 <- getStatesBar(geneticists, "Medical Geneticists", "Number of Medical Geneticists\nper 100,000 population", c("slategray3", "thistle3"))


#Save the plot
gMaps <- ggarrange(g1, g2, g3, g4,
                   nrow = 1, ncol = 4,
                   labels=c("A", "B", "C", "D"),
                   legend = "bottom", common.legend = TRUE)
gMaps <- annotate_figure(gMaps, top = text_grob("", face = "bold", size = 14))
#ggexport(gMaps, filename="figures/workforce-maps.png", width = 4000, height = 1000, res=300)

gBars <- ggarrange(g5, g6, g7, g8,
                   nrow = 1, ncol = 4,
                   labels=c("E", "F", "G", "H"),
                   legend = "bottom", common.legend = TRUE)
gBars <- annotate_figure(gBars, top = text_grob("", face = "bold", size = 14))
#ggexport(gBars, filename="figures/workforce-bars.png", width = 4000, height = 2000, res=300)


gpanel <- ggarrange(gMaps, gBars,
                   nrow = 2, ncol = 1,
                   heights = c(1,2),
                   common.legend = FALSE)
ggexport(gpanel, filename="figures/workforce-panels.png", width = 4000, height = 3000, res=300)


