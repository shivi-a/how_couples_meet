
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(stringr)
library(gt)
library(forcats)
library(plotly)
library(ggthemes)
library(DT)

# Read in data from rds file for both the original dataframe, as well as the
# reshaped version which can be more easily manipulated to analyze meeting types

couples_data <- read_rds("couples_file.rds")

gathered_couples_data <- read_rds("gathered_couples_file.rds")

# Define UI for application, use the shinytheme "sandstone" to increase
# professional appearance (I used the dynamic shiny app theme selector to peruse
# all themes before settling on this one)

ui <- fluidPage(theme = shinytheme("sandstone"),
  
   # Add line breaks for aesthetic purposes
   
   br(),
   
   navbarPage("How Couples Meet and Stay Together",
              
              # Set the first division of the navbar to be couple
              # characteristics - a section that will analyze demographic
              # information from this representative sample of U.S. couples
              
              tabPanel("Couple Characteristics",
         tabsetPanel(
           
           # One of the first questions that came to my mind when playing around
           # with this dataset was one of earnings. Are women today still
           # overall making less than their partners?
           
           tabPanel("Earnings",
                    h3("Who Earns More? Earnings between Partners in the U.S."),
                    br(),
                    sidebarPanel(
                      
                      # The barplot showing respondent's answers can be faceted
                      # according to the respondent's education level -
                      # something that I hypothesized would impact the results,
                      # and that indeed led to different distributions when
                      # varied
                      
                      selectInput("education",
                                  "Respondent Education Level:", levels(couples_data$ppeduc)),
                      
                      # From these visualizations, some trends can be
                      # generalized
                      
                      h6("The majority of men reported making more than their partners at all education levels"),
                      h6("Women were more likely to report making less than their partner, except those with professional or doctorate degrees")
                    ),
                    mainPanel(
                    
                      # This is the actual plot - I initially used ggplot2 for
                      # everything but then found that plotly provided crisper,
                      # sharper looking graphics
                      
                      plotlyOutput("incomePlotly"),
                    br()
                    ),
                    br()
                    ),

           # Create a placeholder tab for visualizations regarding partners'
           # political affliations
           
           # I have created the static versions of these visualizations - they
           # just need to be cleaned up and made dynamic. They indicate that
           # those with a strong partisan affiliation tend to partner with those
           # also with a strong partisan affiliation. Those with no partisan
           # affiliation tend to not partner with someone also Independent or
           # Undecided, but someone weakly identifying with one party or the
           # other
           
           tabPanel("Ages",
                    
                    h3("Who Is Older? Age Differences between Partners"),
                    h6("Pink points indicate female respondents; blue points indicate male respondents"),
                    
                    # After a linebreak for visual purposes, include a related
                    # visualization - a scatterplot of respondent versus
                    # partner ages, displaying potential correlations
                  
                    plotlyOutput("agePlot"),
                    
                    # I had female respondent points be colored pink and male
                    # respondent points be colored blue because I suspected
                    # that the gender of the respondent might influence this
                    # correlation, which it did to a fair degree
                  
                    h4("The presence of more pink dots above the dashed line (y = x) and more blue dots below the line suggests that women tended to report their partners were older than them, while men tended to report their partners were younger than them.")
                    
           ),
           
           tabPanel("Educational Attainment",
                    h3("Do People Tend to Partner with Those of a Similar Educational Background?"),
                    br(),
                    plotlyOutput("educTable")
                    
           ),
           
           tabPanel("Political Affiliation",
                    h3("Do People Tend to Partner with Those of a Similar Political Affiliation?"),
                    br(),
                    plotlyOutput("poliTable")
                    
           )
      )
   ),
tabPanel("How Couples Meet",
         
         # Create a section on the navbar to organize visualization related to
         # how couples meet
      
           tabsetPanel(
             
             # Devote the first tab to a general, broad exploration of the
             # dataset, and most common meeting types overall. Future tweaks
             # include a slider to control the value for the head() function as
             # well as ways to subset the data and see how it affects the most
             # popular meeting types
             
             tabPanel("Meeting Type Frequencies",
                      h3("What Are The Most Common Ways In Which Couples Meet?"),
                      br(),
                      
                      # Include the most common meeting type table for the whole
                      # sample
                      h4("Full Sample"),
                      plotlyOutput("heteroMeetPlot"),
                      DTOutput("meetTable"),
                      
                      br(),
                      
                      # Include the most common meeting type table for the whole
                      # LGBTQ sample
                      
                      h4("LGBTQ Sample"),
                      plotlyOutput("lgbtMeetPlot"),
                      DTOutput("lgbTable"),
                      br()
             ),
             
             # Devote a separate section to exploring how couples who were
             # classmates met. It is possible for couples who were classmates to
             # have met later in life or not in the classroom. I wanted to know
             # how common this was.
             
             tabPanel("Trends Over Time",
                      h3("Frequency of Meeting Types as a Function of Respondent's Age"),
                      plotlyOutput("timeMeet"),
                      h4("Older respondents are more likely to report meeting their partner in the military or on a blind date than younger respondents."),
                      h4("Younger respondents are more likely to report meeting their partner through online gaming or social networks online."),
                      h4("Certain meeting places, such as college and church, have remained consistently frequent among older and younger respondents")
                      ),
             
             # Create a tab with information about when couples meet. One might
             # expect this to be evenly and randomly distributed across the
             # twelve months, but it appears that some months are more
             # serendipitous than others
             
             # Include a tab to organize visualizations related to information
             # about partners' ages
             
             tabPanel("At What Age?",
                      
                      # A particularly relevant question for me is thinking about
                      # at what age one meets "the one" - a future partner.
                      # Creating such a histogram allowed for analysis of possible
                      # trends
                      
                      h3("At What Age Did You Meet Your Partner?"),
                      br(),
                      sidebarPanel(
                        
                        # I hypothesized that the couple identity might affect
                        # this distribution, and therefore made this an option to
                        # segment the data being displayed
                        
                        radioButtons("identity",
                                     "Couple Identity:",
                                     levels(couples_data$w6_same_sex_couple)),
                        
                        # We can describe the insights inferred from this plot
                        
                        h6("The sample size for LGBTQ couples is much smaller, but the histograms nonetheless suggest that same sex couples tend to meet later in life than heterosexual couples")
                      ),
                      mainPanel(
                        
                        # Include the plotly output of the age histogram
                        
                        plotlyOutput("ageHist"),
                        br()
                      )
             ),
             
             tabPanel("At what time of year?",
                      h3("When Do Couples Meet?"),
                      br(),
                      sidebarPanel(
                        
                        # Allows user to dynamically subset the data to a
                        # specific region, and observe firsthand regional
                        # variation
                        
                        radioButtons("region",
                                     "Region:", unique(couples_data$ppreg9)),
                        h5("There is some regional variation as to when couples report first meeting")
                      ),
                      mainPanel(
                        
                        # Input the plot itself into the UI
                        
                      plotlyOutput("meetPlot")
                      ))
         )),

# Create a section with information about when couples marry. One might
# expect this to be evenly and randomly distributed across the
# twelve months, but it appears that some months are more
# serendipitous than others

tabPanel("About",
         mainPanel(
           
           # Provide information about the data source - HCMST 2017
           
           h2("The Data"),
           h5("These visualizations are based on data from the ", a("How Couples Meet and Stay Together 2017 Survey", href="https://data.stanford.edu/hcmst2017"), " , put together by Michael J. Rosenfeld, Reuben J. Thomas, and Sonia Hausen, and part of the SSDS Social Science Data Collection at Stanford University"),
           
           # Ensure that the minimum relevant background is provided to
           # understand what is being presented
           
           h5("This data was collected between July 13 and August 1, 2017, and featured 3,510 survey respondents - a representative sample of English literate adults in the U.S."),
           h5("Questions were asked to subjects with current partners (N=2862) and to subjects with no current partner, but who had a past partner (N=541)"),
           
           # Use the citation for the data provided by the authors to provide
           # proper credit
           
           h5("Citation"),
           h6("Rosenfeld, Michael J., Reuben J. Thomas, and Sonia Hausen. 2019 How Couples Meet and Stay Together 2017 fresh sample. Stanford, CA: Stanford University Libraries."),
           
           # Include information about the app author so that anyone impressed by my Shiny App can contact me to offer me a job
           
           h2("About Me: Shivani Aggarwal"),
           h5("I am a Harvard undergraduate studying biology and passionate about data science."),
           h5("Contact me at saggarwal@college.harvard.edu or connect with me on LinkedIn", a("HERE", href="https://www.linkedin.com/in/s-aggarwal/")),
           
           # Include a link to the Source Code for reproducibility and credibility
           
           h2("Source Code"),
           h5("The source code for this Shiny App can be found at my GitHub", a("HERE", href="https://github.com/shivi-a/how_couples_meet"))
         ))
))

# Define server logic

server <- function(input, output) {
  
  output$incomePlotly <- renderPlotly({
    
    income <- 
      
      # Define the bar plot of the income difference between partners, with the
      # respondent's education level as a dynamic input
      
      couples_data %>% 
      
      filter(ppeduc == input$education) %>% 
      
      # Relevel the factors for this survey question so as to control their
      # plotting order for a more logical interpretation
      
      mutate(Q23 = fct_relevel(Q23, 
                               "We earned about the same amount",
                               "[Partner Name] earned more", 
                               "I earned more")) %>% 
      
      # Minorly tweak the phrasing of one factor - important for professionalism
      
      mutate(Q23 = fct_recode(Q23,"We earned about the same" = "We earned about the same amount", "My partner earned more" = "[Partner Name] earned more")) %>% 
      
      # Similarly tweak the phrasing of this factor to clarify what is being
      # displayed
      
      mutate(ppgender = fct_recode(ppgender, "Male Respondents" = "Male", "Female Respondents" = "Female")) %>% 
      
      # Removed those who refused from the dataset as well as those who reported that their partner was not working for pay -- a small total that do not contribute to any major trends
      
      filter(!is.na(Q23), 
             Q23 != "Refused", 
             Q23 != "[Partner Name] was not working for pay") %>%  
      
      # Create bar chart based on responses for Q23 (respondent's pay versus partner's pay)
      
      ggplot(aes(x = Q23, fill = ppgender)) + 
      
      # The flipped bar chart makes a legend irrelevant
      
      geom_bar(show.legend=FALSE) + 
      
      # Generate two plots, separated by respondent gender, as the variable
      # predicted to affect the result distribution
      
      facet_wrap(~ppgender) + 
      
      coord_flip() + 
      
      # Use theme_few for a more aesthetic, clean plot
      
      theme_few() + 
      
      # Use gender-stereotyped coloring to increase readibility of the plot by
      # playing to audience assumptions
      
      scale_fill_manual(values = c("dodgerblue4", "deeppink4")) +
      
      # Remove redundant labels
      
      labs(x = NULL, y = NULL)
    
    # Convert ggplot to plotly for a sharper output
    
    hide_legend(ggplotly(income)) %>% config(displayModeBar = FALSE) %>% 
      style(hoverinfo = "skip")
    
  })
  
  output$timeMeet <- renderPlotly({
    
    ggplotly(gathered_couples_data %>% 
      filter(meeting_type != "Internet Site") %>% 
      ggplot(aes(x = ppage, fill = meeting_type)) + 
      geom_density() + 
      labs(y = NULL, x = "Respondent's Age") + 
      theme_few() + xlim(0, 100) + 
      facet_wrap(~meeting_type) + 
      theme(legend.position="none")) %>% 
      
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
    
  })
  
  output$meetPlot <- renderPlotly({

    # Dynamically subset the data being fed into the histogram to reflect the
    # user's choices as to region
    
    region_subset <- couples_data %>% filter(Q21A_Month != "Refused", ppreg9 == input$region)
      
      # Draw the bar chart of couples meeting each month of the year
      
      meet <- ggplot(region_subset, aes(x = Q21A_Month)) + 
        
        # Hide the legend for a cleaner plot
        
        geom_bar(show.legend=FALSE) + 
        
        # Use theme_few for a cleaner plot
        
        theme_few() + 
        
        # Add labels to contextualize plot - X and Y axis labels are not needed
        
        labs(x = NULL, y = NULL)
      
      # Convert ggplot2 into plotly for sharper, crisper output
      
      ggplotly(meet) %>% 
        layout(xaxis = list(tickangle = 20)) %>% 
        config(displayModeBar = FALSE) %>% 
        style(hoverinfo = "skip")
    
   })
  
  output$meetTable <- renderDT ({
    
    datatable((gathered_couples_data %>% 
      count(meeting_type) %>% 
      arrange(desc(n))), colnames=c("Meeting Type", "Frequency"), 
      options = list(dom = 'pt'))
    
  })
  
  output$lgbTable <- renderDT ({
    
    datatable((gathered_couples_data %>% 
      filter(xlgb == "LGB sample") %>% 
      count(meeting_type) %>% 
      arrange(desc(n))), colnames=c("Meeting Type", "Frequency"),
      options = list(dom = 'pt'))
    
  })
  
  output$educTable <- renderPlotly ({
  
    ggplotly(couples_data %>% 
        filter(!is.na(w6_q10)) %>% 
        group_by(ppeduc, w6_q10) %>%
        count() %>% group_by(ppeduc) %>%
          ggplot(aes(x = ppeduc, y = fct_rev(w6_q10))) + 
      geom_tile(aes(fill = n), color = "white") + 
      scale_fill_gradient(low = "white", high = "black") + 
      labs(y = "Partner's Highest Level of Education", 
           x = "Respondent's Highest Level of Education", 
           fill = "Count")) %>% 
    layout(xaxis = list(side ="top", tickangle = 15)) %>% 
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
      
  })
  
  output$poliTable <- renderPlotly ({
    
    ggplotly(couples_data %>% 
               filter(!is.na(w6_q12), w6_q12 != "Refused") %>% 
               group_by(partyid7, w6_q12) %>%
               count() %>% ggplot(aes(x = partyid7, y = w6_q12)) + 
               geom_tile(aes(fill = n), color = "white") + 
               scale_fill_gradient(low = "white", high = "black") + 
               labs(y = "Partner's Political Affiliation", 
                    x = "Respondent's Political Affiliation", 
                    fill = "Count")) %>% 
      layout(xaxis = list(side ="top", tickangle = 10)) %>% 
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
  })
  
  output$heteroMeetPlot <- renderPlotly({
    
    ggplotly(gathered_couples_data %>% 
               count(meeting_type) %>% 
               ggplot(aes(x = reorder(meeting_type, -n), y = n)) + 
               geom_col() + 
               labs(x = NULL, y = NULL) +
               theme_few()) %>% 
      layout(xaxis = list(tickangle = 20)) %>% 
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
    
  })
  
  output$lgbtMeetPlot <- renderPlotly({
    
    ggplotly(gathered_couples_data %>% 
               filter(xlgb == "LGB sample") %>% 
               count(meeting_type) %>% 
               ggplot(aes(x = reorder(meeting_type, -n), y = n)) + 
               geom_col() + 
               labs(x = NULL, y = NULL) +
               theme_few()) %>% 
      layout(xaxis = list(tickangle = 20)) %>% 
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
    
  })
  
  output$ageHist <- renderPlotly({

    # Generate a histogram of the age of respondents when they met their partner
    
    gg <- couples_data %>% 
      
      # Allow the data being displayed to be dynamically subsetted to either
      # just heterosexual couples or same-sex couples. I hypothesized that this
      # characteristic might affect the age distribution of respondents when
      # they met their other halves for the first time, which turned out to be
      # true
      
      filter(w6_same_sex_couple == input$identity) %>% 
      
      # Generate a histogram, and use a binwidth of 1 so that each bin
      # corresponds to 1 year - the most intuitive grouping
      
      ggplot(aes(x = age_when_met)) + 
      
      geom_histogram(binwidth = 1) + 
      
      # I like the histogram for its ability to capture exactly where there is
      # data and where there isn't data - the gaps and the spikes. For the
      # final, I am consider switching to a geom_density plot. This might be
      # more aesthetically pleasing because of the smoothed distributions
      # e.g. geom_density(adjust = 1/4, alpha = 0.1)
      
      labs(y = NULL, x = "Respondent's Age At First Meeting") +
      
      # Add more frequent x-axis labels to better contextualize an axis about
      # ages +
      
      xlim(0, 85) +
      
      # Use theme_few for a cleaner, consistent output
      
      theme_few()
    
    # Convert ggplot2 graphic to plotly for a crisper, sharper output
    
    ggplotly(gg) %>%
      
      config(displayModeBar = FALSE) %>% 
      
      style(hoverinfo = "skip")
    
  })
  
  output$agePlot <- renderPlotly({
    
    # Create graphic to show the relationship between respondent's age and their partner's age -- are most reporting partners who are older than them? How does this differ according to the sex of the respondent?
    
    age <- couples_data %>% 
      
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
    
    # Convert ggplot to plotly and hide the legend - uneccessary due to figure
    # caption
    
    hide_legend(ggplotly(age)) %>% 
      
      config(displayModeBar = FALSE) %>% 
      
      style(hoverinfo = "skip")
  
  })
  
}

# Run the application 

shinyApp(ui = ui, server = server)

