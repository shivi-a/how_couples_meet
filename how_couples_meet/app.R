
library(shiny)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gt)
library(lubridate)
library(forcats)
library(ggthemes)

# Read in data from rds file

couples_data <- read_rds("couples_file.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Sidebar with a slider input for number of bins 
   br(),
   
   navbarPage("How Couples Meet and Stay Together",
              tabPanel("Couple Characteristics",
   sidebarLayout(
      sidebarPanel(
         radioButtons("region",
                     "Region:", unique(couples_data$ppreg9))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel("About",
                    br(),
                    h6("Thanks to data from the ", a("ShotSpotter Project", href="http://justicetechlab.org/shotspotter-data/"), " by the Justice Tech Lab, we can visualize gunshot data in Camden, NJ")
                    ),
           tabPanel("When Couples Meet", 
                    plotOutput("meetPlot")),
           tabPanel("Marry",
                    plotOutput("marryPlot"))
         )
      )
   )
),
tabPanel("Meetings",
         mainPanel(
           h2("Meetings")
         )),
tabPanel("Marriage",
         mainPanel(
           h2("Marriage")
         )),
tabPanel("About",
         mainPanel(
           h2("SSDS Social Science Data Collection"),
           h5("Thanks to data from the ", a("Social Science Data Collection", href="https://data.stanford.edu/hcmst2017"), " at Stanford University, we can understand how couples meet"),
           br(),
           h2("Source Code"),
           h5("The source code for this Shiny App can be found", a("HERE", href="https://github.com/shivi-a/how_couples_meet"))
         ))
))

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$meetPlot <- renderPlot({

      region_subset <- couples_data %>% filter(Q21A_Month != "Refused", ppreg9 == input$region)
      
      # draw the histogram with the specified number of bins
      
      ggplot(region_subset, aes(x = Q21A_Month, fill = Q21A_Month)) + 
        
        # Hide the legend for a cleaner plot
        
        geom_bar(show.legend=FALSE) + 
        
        # Use theme_few for a cleaner plot
        
        theme_few() + 
        
        # Add a horizontal line at the value representing the number of couples in this dataset that should be marrying each month if marriages were equally distributed - allows one to better understand what is a deviation from this null hypothesis
        
        #geom_hline(yintercept=avg_meet_per_month, linetype="dashed") + 
        
        # Add labels to contextualize plot - X and Y axis labels are not needed
        
        labs(x = NULL, y = NULL)
    
   })
  
  output$marryPlot <- renderPlot({
    
    region_subset <- couples_data %>% filter(Q21D_Month != "Refused", ppreg9 == input$region)
    
    # draw the histogram with the specified number of bins
    
    ggplot(region_subset, aes(x = Q21D_Month, fill = Q21D_Month)) + 
      
      # Hide the legend for a cleaner plot
      
      geom_bar(show.legend=FALSE) + 
      
      # Use theme_few for a cleaner plot
      
      theme_few() + 
      
      # Add a horizontal line at the value representing the number of couples in this dataset that should be marrying each month if marriages were equally distributed - allows one to better understand what is a deviation from this null hypothesis
      
      #geom_hline(yintercept=avg_meet_per_month, linetype="dashed") + 
      
      # Add labels to contextualize plot - X and Y axis labels are not needed
      
      labs(x = NULL, y = NULL)
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

