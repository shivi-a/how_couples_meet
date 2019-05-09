
# Load neccessary dependencies

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

            # I also was curious about the relationship between the different
            # ages of two people in a relationship. It is a common intuition
            # that women are the younger ones in their relationships and men are
            # the older ones. Making this scatterplot was an attempt to confirm
            # or deny this intuition
           
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
           
           # Create a tab for visualizations regarding partners'
           # educational attainment
           
           # Thanks to advice from Preceptor, I chose a heatmap to visualize
           # this data. Those with the highest education levels (e.g. Doctorate)
           # are very unlikely to partner with someone of a low education level.
           # Those of lower education attaninment also tend to partner with
           # those of a similar educational attainment
           
           # This graphic was also motivated by the changes observed in
           # responses to the income question based on respondent education
           # level. I wanted to see if those responses could potentially be
           # confounded by changes in partner education level
           
           tabPanel("Educational Attainment",
                    
                    h3("Do People Tend to Partner with Those of a Similar Educational Background?"),
                    
                    br(),
                    
                    plotlyOutput("educTable")
                    
           ),
           
           # Create a tab for visualizations regarding partners'
           # political affliations
           
           # Thanks to advice from Preceptor, I chose a heatmap to visualize
           # this data. It indicates that those with a strong partisan
           # affiliation tend to partner with those also with a strong partisan
           # affiliation. Those with no partisan affiliation tend to not partner
           # with someone also Independent or Undecided, but someone weakly
           # identifying with one party or the other
           
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
             # dataset, and most common meeting types overall. 
             
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
                      # LGBTQ sample - as a comparison, the ranking of meeting
                      # types changes, and different meeting types move up in
                      # "importance" or frequency
                      
                      h4("LGBTQ Sample"),
                      
                      plotlyOutput("lgbtMeetPlot"),
                      
                      DTOutput("lgbTable"),
                      
                      br()
             ),
             
             # Devote a section to understand how trends in couples meeting has
             # changed over time (using respondent ages as a proxy for time).
             # The full sample involves respondents from a variety of ages, thus
             # capturing a representative sample of the nation, but not
             # necessarily of the kind of meetings that will characterize the
             # next generation.
             
             # By plotting the frequency of each meeting type as a function of
             # respondent age, we could see that some meeting types are
             # consistently reported among young and old respondents, likely to
             # still hold true for future generations, while other meeting types
             # like the military are only so common among older respondents.
             # Logically, meeting types like social networks spike among younger
             # respondents
             
             tabPanel("Trends Over Time",
                      
                      h3("Frequency of Meeting Types as a Function of Respondent's Age"),
                      
                      plotlyOutput("timeMeet"),
                      
                      h4("Older respondents are more likely to report meeting their partner in the military or on a blind date than younger respondents."),
                      
                      h4("Younger respondents are more likely to report meeting their partner through online gaming or social networks online."),
                      
                      h4("Certain meeting places, such as college and church, have remained consistently frequent among older and younger respondents")
                      ),
             
             # Include a tab to organize visualizations related to information
             # about partners' ages
             
             tabPanel("At What Age?",
                      
                      # A particularly relevant question for me is thinking about
                      # at what age one meets "the one" - a future partner.
                      # Creating such a histogram allowed for analysis of possible
                      # trends
                      
                      # I strongly debated using an overlapping histogram for
                      # this visual, however ultimately decided against it
                      # (making sure to nonetheless fix the x-axis scale between
                      # the plots to allow for comparisons). The reason I
                      # decided against it was because ultimately, the sample
                      # size for the heterosexual couples was far larger than
                      # that of the same-sex couples, meaning that in an
                      # overlapping histogram, the distribution of counts for
                      # the same-sex couples was not so clear as a trend.
                      # Overlapping density curves were also a possibility,
                      # however, I like prefered the granularity of a histogram
                      # for this data, where this is a logical bin size of 1
                      
                      
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
             
             # Create a tab with information about when couples meet. One might
             # expect this to be evenly and randomly distributed across the
             # twelve months, but it appears that some months are more
             # serendipitous than others, and that this variation is region-specific
             
             tabPanel("At what time of year?",
                      
                      h3("When Do Couples Meet?"),
                      br(),
                      
                      sidebarPanel(
                        
                        # Allows user to dynamically subset the data to a
                        # specific region, and observe firsthand regional
                        # variation
                        
                        radioButtons("region",
                                     "Region:", unique(couples_data$ppreg9)),
                        
                        h5("The reported first meetings between couples are not uniformly distributed across all months of the year"),
                        h5("There is some regional variation as to when couples report first meeting")
                      
                        ),
                      
                      mainPanel(
                        
                        # Input the plot itself into the UI
                        
                      plotlyOutput("meetPlot")
                      ))
         )),

# Include an About” tab with name, contact information, GitHub repo and data
# source information.

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
               
      # For the sake of having a complete grid of possible meeting types to
      # display (and not with one facet awkwardly hanging off), I made the
      # decision to drop one of the Internet-related meeting sub-types, as I
      # felt that its trend as a function of respondent age was captured by some
      # of the other Internet-related meeting types
        
      filter(meeting_type != "Internet Site") %>% 
      
        ggplot(aes(x = ppage, fill = meeting_type)) + 
        
        # Use density curves as they will provide comparable distributions
        # calculated based on frequency and not total count, thus allowing for
        # comparison despite differences in the total count per meeting type
      
        geom_density() + 
      
        labs(y = 'Frequency', x = "Respondent's Age") + 
      
        # Use a ggtheme for a more consistent aesthetic, consistent across the app
        
        # Fix the x-axis limits to include a logical range of ages
        
        theme_few() + xlim(0, 100) + 
      
        # Produce a plot for each meeting type
        
        facet_wrap(~meeting_type) + 
      
        # Attempt to increase the horizontal spacing between rows of facet plots
        # - some weird formatting / overlap was observed due to ggplotly()
        # behavior
        
        theme(legend.position="none",
            panel.spacing.y=unit(0.75, "lines"))) %>% 
      
      # Remove distracting ModeBar and hoverinfo - I find personally that most
      # of the options plotly provides just lead to unhelpful extreme zooming in
      # or movement of the data out of visual range -- I hoped to limit such
      # manipulations
      
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
        
        # Ensure that the x-axis labels do not overlap, and remove distracting
        # ModeBar and hoverinfo that in this context do not provide any useful
        # information and tend to lead to the plot being messed up
        
        layout(xaxis = list(tickangle = 20)) %>% 
        config(displayModeBar = FALSE) %>% 
        style(hoverinfo = "skip")
    
   })
  
  output$meetTable <- renderDT ({
    
    datatable((gathered_couples_data %>% 
                 
      count(meeting_type) %>% 
        
      # Arrange the table so as to provide the more meaningful meeting types for
      # this population first
        
      arrange(desc(n))), colnames=c("Meeting Type", "Frequency"),
      
      # Remove the search bar option from the table output - a distraction that
      # provides no benefit in this situation
      
      options = list(dom = 'pt'))
    
  })
  
  output$lgbTable <- renderDT ({
    
    datatable((gathered_couples_data %>% 
                 
      filter(xlgb == "LGB sample") %>% 
        
      count(meeting_type) %>% 
      
        # Arrange the table so as to provide the more meaningful meeting types
        # for this population first
        
      arrange(desc(n))), colnames=c("Meeting Type", "Frequency"),
      
      # Remove the search bar option from the table output - a distraction that
      # provides no benefit in this situation
      
      options = list(dom = 'pt'))
    
  })
  
  output$educTable <- renderPlotly ({
  
    ggplotly(couples_data %>% 
               
        filter(!is.na(w6_q10)) %>% 
          
        # Create a two-way table for each combination of respondent and partner
        # education level
          
        group_by(ppeduc, w6_q10) %>%
          
        count() %>% 
          
          ggplot(aes(x = ppeduc, y = fct_rev(w6_q10))) + 
          
      # Use geom_tile to create a heatmap of the counts for each category
          
      geom_tile(aes(fill = n), color = "white") + 
      
      # I considered using another color rather than black to indicate
      # intensity, but ultimately decided that the connotations of a different
      # color might distract from the implications of the figure
        
      scale_fill_gradient(low = "white", high = "black") + 
      
        labs(y = "Partner's Highest Level of Education", 
           x = "Respondent's Highest Level of Education", 
           fill = "Count")) %>% 
    
      # Ensure that x-axis tick labels do not overlap, and remove the
      # distracting ModeBar and hover info that do not add in this particular
      # situation, instead distracting from the plot output
      
      layout(xaxis = list(side ="top", tickangle = 15)) %>% 
      
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
      
  })
  
  output$poliTable <- renderPlotly ({
    
    ggplotly(couples_data %>% 
               
               filter(!is.na(w6_q12), w6_q12 != "Refused") %>% 
               
               # Create a two-way table for each combination of respondent and partner
               # political affiliation
               
               group_by(partyid7, w6_q12) %>%
               
               count() %>% 
               
               ggplot(aes(x = partyid7, y = w6_q12)) + 
               
               # Use geom_tile to create a heatmap of the counts for each
               # category
               
               geom_tile(aes(fill = n), color = "white") + 
               
               # I considered using another color rather than black to indicate
               # intensity, but ultimately decided that the connotations of a different
               # color might distract from the implications of the figure
               
               scale_fill_gradient(low = "white", high = "black") + 
               
               labs(y = "Partner's Political Affiliation", 
                    x = "Respondent's Political Affiliation", 
                    fill = "Count")) %>% 
      
      # Ensure that x-axis tick labels do not overlap, and remove the
      # distracting ModeBar and hover info that do not add in this particular
      # situation, instead distracting from the plot output
      
      layout(xaxis = list(side ="top", tickangle = 10)) %>% 
      
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
  })
  
  output$heteroMeetPlot <- renderPlotly({
    
    ggplotly(gathered_couples_data %>% 
               count(meeting_type) %>% 
               
               # Generate bar chart of the frequency of each meeting type for
               # the given population - a more visual representation of what is
               # displayed in the corresponding table - and ensure that the
               # factors displayed are in order from most prevalent to least
               # prevalent so that the most common and least common meeting
               # types can be compared
               
               ggplot(aes(x = reorder(meeting_type, -n), y = n)) + 
               
               geom_col() + 
               
               # Remove x and y labels as superfluous in this context
               
               labs(x = NULL, y = NULL) +
               
               theme_few()) %>% 
      
      # Ensure that x-axis labels do not overlap and that the distracting
      # ModeBar and hover info are not displayed
      
      layout(xaxis = list(tickangle = 20)) %>% 
      config(displayModeBar = FALSE) %>% style(hoverinfo = "skip")
    
  })
  
  output$lgbtMeetPlot <- renderPlotly({
    
    ggplotly(gathered_couples_data %>%
               
               filter(xlgb == "LGB sample") %>% 
               
               count(meeting_type) %>% 
               
               # Generate bar chart of the frequency of each meeting type for
               # the given population - a more visual representation of what is
               # displayed in the corresponding table - and ensure that the
               # factors displayed are in order from most prevalent to least
               # prevalent so that the most common and least common meeting
               # types can be compared
               
               ggplot(aes(x = reorder(meeting_type, -n), y = n)) + 
               
               geom_col() + 
               
               # Remove x and y labels as superfluous in this context
               
               labs(x = NULL, y = NULL) +
               
               theme_few()) %>% 
      
      # Ensure that x-axis labels do not overlap and that the distracting
      # ModeBar and hover info are not displayed
      
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
      
      # Ensure that x-axis labels do not overlap and that the distracting
      # ModeBar and hover info are not displayed
      
      config(displayModeBar = FALSE) %>% 
      
      style(hoverinfo = "skip")
  
  })
  
}

# Run the application 

shinyApp(ui = ui, server = server)

