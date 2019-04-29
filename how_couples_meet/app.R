
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gt)
library(lubridate)
library(forcats)
library(ggthemes)

# Read in data from rds file

couples_data <- read_rds("couples_file.rds")

gathered_couples_data <- read_rds("gathered_couples_file.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("sandstone"),
  
   br(),
   
   navbarPage("How Couples Meet and Stay Together",
              tabPanel("Couple Characteristics",
         tabsetPanel(
           tabPanel("Earnings",
                    h3("Who Earns More? Earnings between Partners in the U.S."),
                    br(),
                    sidebarPanel(
                      selectInput("education",
                                  "Respondent Education Level:", unique(couples_data$ppeduc)),
                      h6("The majority of men reported making more than their partners at all education levels"),
                      h6("Women were more likely to report making less than their partner, except those with professional or doctorate degrees"),
                      h6("Add about same-sex couples?")
                    ),
                    mainPanel(
                    plotOutput("incomePlot"),
                    plotlyOutput("incomePlotly"),
                    br()
                    )),
           tabPanel("Age",
                    h3("At What Age Did you Meet Your Partner?"),
                    plotlyOutput("ageHist"),
                    plotOutput("agePlot"),
                    h4("Points on the dashed line indicate respondents reporting a partner of the same age. Women tended to report older partners, while men reported younger partners"),
                    br(),
                    h3("Age differences - Calculated")
           ),
           tabPanel("Political Affiliation",
                    h3("Correlations in Partner Ages"),
                    h4("Points on the dashed line indicate respondents reporting a partner of the same age. Women tended to report older partners, while men reported younger partners"),
                    br(),
                    h3("Age differences - Calculated")
           )
      )
   ),
tabPanel("Meet",
           tabsetPanel(
             tabPanel("Types",
                      h3("Types of Meetings"),
                      br(),
                      tableOutput("meetTable"),
                      tableOutput("lgbTable")
                      #strangers
             ),
             tabPanel("Classmates",
                      h3("Did partners who went to the same high school or college meet as classmates?"),
                      br(),
                      h5("Couples who went to the same high school (different colleges)"),
                      tableOutput("schoolTable"),
                      br(),
                      h5("Couples who went to the same college (different high schools)"),
                      tableOutput("collegeTable"),
                      br(),
                      h5("Couples who went to the same high school and college"),
                      tableOutput("classTable")
                      
             ),
             tabPanel("When",
                      br(),
                      sidebarPanel(
                        radioButtons("region",
                                     "Region:", unique(couples_data$ppreg9))
                      ),
                      mainPanel(
                      plotOutput("meetPlot")
                      ),
                      h2("time to relationship start - calculate, ppincimp, stuff with interracial, political id, race"))
         )),
tabPanel("Marry",
         h3("When Do Couples Marry?"),
         br(),
         sidebarPanel(
           radioButtons("region",
                        "Region:", unique(couples_data$ppreg9)),
           h4("Among those married, how did they meet"),
           h4("How old when you married")
           ),
         mainPanel(
           plotOutput("marryPlot")
         )),
tabPanel("Stay Together",
         mainPanel(
           h2("Stay Together"),
           h4("The previous data involves six waves of data")
         )),
tabPanel("About",
         mainPanel(
           h2("The Data"),
           h5("Thanks to data from the ", a("Social Science Data Collection", href="https://data.stanford.edu/hcmst2017"), " at Stanford University, we can understand trends among couples and how they meet in the US"),
           h5("This data was collected between July 13 and August 1, 2017, and featured 3,510 survey respondents - a representative sample of English literate adults in the US"),
           h5("Citation"),
           h6("Rosenfeld, Michael J., Reuben J. Thomas, and Sonia Hausen. 2019 How Couples Meet and Stay Together 2017 fresh sample. Stanford, CA: Stanford University Libraries."),
           h2("Shiny App Developed by Shivani Aggarwal"),
           h5("Contact me at saggarwal@college.harvard.edu"),
           h2("Source Code"),
           h5("The source code for this Shiny App can be found", a("HERE", href="https://github.com/shivi-a/how_couples_meet"))
         ))
))

# Define server logic required to draw a histogram

server <- function(input, output) {
  
  output$incomePlotly <- renderPlotly({
    
    income <- 
      
      couples_data %>% 
      
      filter(ppeduc == input$education) %>% 
      
      mutate(Q23 = fct_relevel(Q23, 
                               "We earned about the same amount",
                               "[Partner Name] earned more", 
                               "I earned more")) %>% 
      
      mutate(Q23 = fct_recode(Q23,"We earned about the same" = "We earned about the same amount", "My partner earned more" = "[Partner Name] earned more")) %>% 
      
      mutate(ppgender = fct_recode(ppgender, "Male Respondents" = "Male", "Female Respondents" = "Female")) %>% 
      
      # Removed those who refused from the dataset as well as those who reported that their partner was not working for pay -- a small total that do not contribute to any major trends
      
      filter(!is.na(Q23), 
             Q23 != "Refused", 
             Q23 != "[Partner Name] was not working for pay") %>%  
      
      # Create bar chart based on responses for Q23 (respondent's pay versus partner's pay)
      
      ggplot(aes(x = Q23, fill = ppgender)) + 
      
      geom_bar(show.legend=FALSE) + 
      
      facet_wrap(~ppgender) + 
      
      coord_flip() + 
      
      theme_few() + 
      
      scale_fill_manual(values = c("dodgerblue4", "deeppink4")) +
      
      labs(x = NULL, y = NULL)
      
    hide_legend(ggplotly(income))
    
  })

  output$incomePlot <- renderPlot({
    
    couples_data %>% 
      
      filter(ppeduc == input$education) %>% 
    
      mutate(Q23 = fct_relevel(Q23, 
                               "We earned about the same amount",
                               "[Partner Name] earned more", 
                               "I earned more")) %>% 
      
      mutate(Q23 = fct_recode(Q23,"We earned about the same" = "We earned about the same amount", "My partner earned more" = "[Partner Name] earned more")) %>% 
      
      mutate(ppgender = fct_recode(ppgender, "Male Respondents" = "Male", "Female Respondents" = "Female")) %>% 
      
      # Removed those who refused from the dataset as well as those who reported that their partner was not working for pay -- a small total that do not contribute to any major trends
      
      filter(!is.na(Q23), 
             Q23 != "Refused", 
             Q23 != "[Partner Name] was not working for pay") %>%  
      
      # Create bar chart based on responses for Q23 (respondent's pay versus partner's pay)
      
      ggplot(aes(x = Q23, fill = ppgender)) + 
      
      geom_bar(show.legend=FALSE) + 
      
      facet_wrap(~ppgender) + 
      
      coord_flip() + 
      
      theme_few() + 
      
      scale_fill_manual(values = c("dodgerblue4", "deeppink4")) +
      
      labs(x = NULL, y = NULL)
    
  })
  
  output$meetPlot <- renderPlot({

      region_subset <- couples_data %>% filter(Q21A_Month != "Refused", ppreg9 == input$region)
      
      # draw the histogram with the specified number of bins
      
      ggplot(region_subset, aes(x = Q21A_Month)) + 
        
        # Hide the legend for a cleaner plot
        
        geom_bar(show.legend=FALSE) + 
        
        # Use theme_few for a cleaner plot
        
        theme_few() + 
        
        # Add a horizontal line at the value representing the number of couples in this dataset that should be marrying each month if marriages were equally distributed - allows one to better understand what is a deviation from this null hypothesis
        
        #geom_hline(yintercept=avg_meet_per_month, linetype="dashed") + 
        
        # Add labels to contextualize plot - X and Y axis labels are not needed
        
        labs(x = NULL, y = NULL)
    
   })
  
  output$schoolTable <- renderTable({
    
    gathered_couples_data %>% 
      filter(Q25 == "Same High School", 
             Q26 == "Did not attend same college or university") %>% 
      count(meeting_type) %>% 
      arrange(desc(n)) %>% 
      head(5)
      
    #divide by total number to get percentages then table or plot
  })
  
  output$collegeTable <- renderTable({
    
    gathered_couples_data %>% 
      filter(Q26 == "Attended same college or university", 
             Q25 == "Different High School") %>% 
      count(meeting_type) %>% 
      arrange(desc(n)) %>% 
      head(5)
    
  })
  
  output$classTable <- renderTable({
    
    gathered_couples_data %>% 
      filter(Q26 == "Attended same college or university", 
             Q25 == "Same High School") %>% 
      count(meeting_type) %>% 
      arrange(desc(n)) %>% 
      head(5)
    
  })
  
  output$meetTable <- renderTable ({
    
    gathered_couples_data %>% 
      count(meeting_type) %>% 
      arrange(desc(n))
    
  })
  
  output$lgbTable <- renderTable ({
    
    gathered_couples_data %>% 
      filter(xlgb == "LGB sample") %>% 
      count(meeting_type) %>% 
      arrange(desc(n))
    
  })
  
  output$ageHist <- renderPlotly({
    
    # FIX formatting
    
    gg <- couples %>% 
      ggplot(aes(x = age_when_met)) + 
      geom_histogram(binwidth = 1) + 
      labs(y = NULL) +
      theme_few()
    
    ggplotly(gg)
    
  })
  
  
  output$agePlot <- renderPlot({
    
    # Create graphic to show the relationship between respondent's age and their partner's age -- are most reporting partners who are older than them? How does this differ according to the sex of the respondent?
    
    couples_data %>% 
      
      # Remove those who refused to answer this question (coded by a value of "-1")
      filter(Q9 != -1) %>% 
      
      # Create a scatterplot of respondent's age versus partner's age, and colored by the respondent's gender
      
      ggplot(aes(x = ppage, y = Q9, color = ppgender)) +
      
      # Make points translucent to allow for visualization of overlapping points
      
      geom_point(alpha = 0.6) + 
      
      # Change the color scale to match more intuitive designations of male and female
      
      scale_color_manual(values = c("dodgerblue", "deeppink"), name = "Respondent's Sex") +
      
      # Add labels to contextualize the plot
      
      labs(x = "Respondent's Age", 
           y = "Partner's Age") + 
      
      # Add a line with a slope of 1 -- points that lie on this line represent couples where the two are the same age -- above it, the partner is older, and below, the respondent is older
      
      geom_abline(slope = 1, linetype = "dashed") + 
      
      # Use theme_few for a cleaner plot
      
      theme_few() + 
      
      # Expand the limits of the default view so that the x and y axes are proportional, both showing 10 to 90
      
      expand_limits(x = 10:90) + 
      
      # Ensure that breaks are shown for the x and y axis in intervals of 10, as we tend to think of age in decades
      
      scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
      
      scale_y_continuous(breaks = seq(0, 100, by = 10))
  
  })
  
  output$marryPlot <- renderPlot({
    
    region_subset <- couples_data %>% filter(Q21D_Month != "Refused", ppreg9 == input$region)
    
    # draw the histogram with the specified number of bins
    
    ggplot(region_subset, aes(x = Q21D_Month)) + 
      
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

