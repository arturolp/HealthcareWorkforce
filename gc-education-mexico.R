###################################################################
#
# Genetic Counseling Assessment of Mexico
#
# Author: Arturo Lopez Pineda <arturolp@stanford.edu>
#
# Date: Oct 8, 2018
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


#----------------------------------
# 1. Load data
#----------------------------------

states <- read.csv("infostates-mexico.csv")
universities <- read.csv("info-medshools-mexico.csv")

#----------------------------------
# 2. Plot data
#----------------------------------

regions = c("North" = "#4B5CAB", 
            "Central East" = "#D16B69",
            "Central West" = "#69BCE8",
            "South" = "#8CB838",
            "South East" = "#F2A03D")

states$newX = scale(states$clinical.genetics.courses, center = TRUE, scale = FALSE)
states$newY = scale(states$genetics.courses, center = TRUE, scale = FALSE)

g1<- ggplot(states, aes(x=clinical.genetics.courses, y=genetics.courses)) +
  geom_point(aes(colour=region, size = medical.students)) +
  geom_text_repel(aes(label=code), color="gray40", size=2, segment.size = 0.2, force=1, box.padding=0.25, segment.color="gray77") +
  scale_color_manual(values=regions, name="Regions") +
  scale_x_continuous(limits=c(-5000,15000), breaks=c(0, 5000, 10000, 15000))+
  #scale_y_log10(breaks=c(0, 500, 1000, 5000, 15000))+
  scale_size_continuous(name="Total Enrollment", range = c(0, 8)) +
  labs(x="Students enrolled in\nClinical Genetics-related courses",
       y="Students enrolled in\nGenetics-related courses")+
  theme_light() +
  theme(legend.position = "right",
        panel.grid.minor = element_blank(),
        #panel.grid.major = element_blank(),
        aspect.ratio = 1,
        plot.subtitle = element_text(hjust=0.5, face="bold")
  )

gstates <- annotate_figure(g1, top = text_grob("Medical Education", face = "bold", size = 14))
ggexport(gstates, filename="figures/medical-education.png", width = 2000, height = 1500, res=300)


#--------


shortName <- function(name){
  
  name = str_replace(name, "Universidad|Universitarios", "U.")
  name = str_replace(name, "Instituto", "I.")
  name = str_replace(name, "Centro", "C.")
  name = str_replace(name, "Autonoma|Autonomo", "A.")
  name = str_replace(name, "Tecnologico|Tecnologica", "T.")
  name = str_replace(name, "Estudios", "E.")
  name = str_replace(name, "Superiores", "S.")
  name = str_replace(name, "Estado", "E.")
  name = str_replace(name, "Ciencias", "C.")
  name = str_replace_all(name, "de |y |del ", "")
  name = str_replace(name, "Michoacana", "Mich.")
  name = str_replace(name, "San", "Sn.")
  name = str_replace(name, "Nicolas", "Nic.")
  name = str_replace(name, "Chavez", "Ch.")
  
  return(name)
}


plotRegionCourses <- function(mydata, title){
  
  mynewData = cbind(name=shortName(mydata$university), 
                    courses=as.numeric(mydata$medical.genetics.courses), 
                    type=rep("Clinical Genetics", dim(mydata)[1]),
                    money=as.character(mydata$type)
                    )
  mynewData = rbind(mynewData, cbind(name=shortName(mydata$university), 
                                     courses=as.numeric(mydata$genetics.courses-mydata$medical.genetics.courses), 
                                     type=rep("Non-clinical Genetics", dim(mydata)[1]),
                                     money=as.character(mydata$type)
                                     )
                    )
  mynewData = as.data.frame(mynewData)
  mynewData$courses = as.numeric(as.character(mynewData$courses))
  mynewData = unique(mynewData)
  
  gg<- ggplot(mynewData)  +
    geom_bar(aes(x=reorder(name, courses, sum), y=courses, fill=type), stat="identity") +
    scale_fill_manual(values=c("#E1B378", "#5F9EA0"), name="Course type") +
    scale_y_continuous(limits=c(0,4)) +
    coord_flip() +
    labs(subtitle=title, y="Number of courses", x="University") +
    theme_light() +
    theme(legend.position = "bottom",
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.subtitle = element_text(hjust=0.5, face="bold"),
          aspect.ratio = 2
    ) +
    facet_grid(money ~  ., scales = "free", space="free") 
    #facet_wrap(region ~ money, scales = "free", ncol=2)
  
  return(gg)
}


north = as.vector(subset(states, region=="North")[, "name"])
gN <- plotRegionCourses(subset(universities, state %in% north), "North region")

centralE = as.vector(subset(states, region=="Central East")[, "name"])
gCE <- plotRegionCourses(subset(universities, state %in% centralE), "Central East region")

centralW = as.vector(subset(states, region=="Central West")[, "name"])
gCW <- plotRegionCourses(subset(universities, state %in% centralW), "Central West region")

south = as.vector(subset(states, region=="South")[, "name"])
gS <- plotRegionCourses(subset(universities, state %in% south), "South region")

southE = as.vector(subset(states, region=="South East")[, "name"])
gSE <- plotRegionCourses(subset(universities, state %in% southE), "South East region")


gBars <- ggarrange(gN, gCE, gCW, gS, gSE,
                   nrow = 1, ncol = 5,
                   align="hv",
                   legend = "bottom", common.legend = TRUE)
gBars <- annotate_figure(gBars, top = text_grob("", face = "bold", size = 14))
ggexport(gBars, filename="figures/schools-by-region.png", width = 5000, height = 2000, res=300)

