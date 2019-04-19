
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
   
   # Application title
   titlePanel("How Couples Meet"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         radioButtons("region",
                     "Region:", unique(couples_data$ppreg9))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         tabsetPanel(
           tabPanel("When Couples Meet", 
                    plotOutput("meetPlot")),
           tabPanel("Marry",
                    plotOutput("marryPlot"))
         )
      )
   )
)

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

