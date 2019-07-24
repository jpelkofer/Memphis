#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(janitor)
library(ggthemes)

theme_set(theme_fivethirtyeight())


PGA <- read.csv("PGA.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Compare Golfers Strokes Gained Approach Distributions"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput("player",
                     "Player:",
                     choices = PGA$player,
                     selected = "Justin Thomas",
                     multiple = TRUE)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot", height = "700px")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     PGA %>%
       clean_names() %>%
       filter(player %in% c(input$player), !is.na(sg_app)) %>%
       add_count(player) %>%
       mutate(plyr_sample = paste0(player, " (", as.character(n), ")" )) %>%
       ggplot(aes(x=fct_reorder(plyr_sample, sg_app, fun=median), y = sg_app)) +
       geom_boxplot(fill = '#F57921', color = '#472F91') + 
       labs(title = "Distributions of Strokes Gained on Approach Shots",
            subtitle = "2018-2019 PGA Season (sample size)",
            caption = "Data Source: advancedsportsanalytics.com") +
       theme(panel.background = element_rect(fill = 'white'),
             plot.background = element_rect(fill = 'white'),
             plot.title = element_text(color = '#472F91'),
             plot.subtitle = element_text(color = '#472F91', size=12),
             plot.caption = element_text(color = "#472F91"),
             axis.text = element_text(color = '#472F91', size = 12)) +
       coord_flip()
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

