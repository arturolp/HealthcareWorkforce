#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library("mxmaps")
library(ggrepel)

# Define UI for application that draws a histogram
ui <- fluidPage(
  plotOutput("plot", click = "plot_click", width = "100%", height = "700px"),
  verbatimTextOutput("info")
)

# Define Data
data(mxstate.map)
states <- mxstate.map
centers <- read.csv("/Users/arturolp/Dropbox/Daiana-GCMexico/R/states-mexico-centers.csv")


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$plot <- renderPlot({
    ggplot(states) +
      geom_polygon(aes(long, lat, fill=region, group=group), color = "white", size = 0.8) +
      guides(fill=FALSE) +
      geom_text_repel(data=centers, aes(long, lat, label=code)) +
      coord_map() +
      #coord_fixed(1.3) +
      theme_light() +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            panel.grid.major = element_blank()
            #axis.title = element_blank(),
            #axis.ticks = element_blank(),
            #axis.text = element_blank()
      )
  })
  
  output$info <- renderText({
    xy_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste(e$x, e$y, sep=", ")
    }
    
    paste0(
      "click: x,y = ", xy_str(input$plot_click)
      )
    
  })  
}


# Run the application 
shinyApp(ui = ui, server = server)

