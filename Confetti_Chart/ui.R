
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(ggvis)

shinyUI(fluidPage(

  # Application title
  titlePanel("Class View"),
  fluidRow(
    a('Switch to Student View', href='http://paulxu.shinyapps.io/udio_student', target='_self')
  ),

    #uiOutput('ggvis_ui'),
  ggvisOutput('confetti'),
  
  
  hr(),
  
  fluidRow(
    column(3,
           selectInput("teacher", label = h4("Choose a Class by Teacher Number"), 
                       choices = list("Teacher 1" = 1, "Teacher 2" = 2, "Teacher 3" = 3,
                                      "Teacher 4" = 4, "Teacher 5" = 5, "Teacher 6" = 6,
                                      "Teacher 7" = 7, "Teacher 9" = 9, "Teacher 11" = 11), 
                       selected = 7),
           sliderInput("elapsed", label = h4("Choose a Time Range"), min = 0, 
                       max = 3600, value = c(0, 3600)),
           dateRangeInput("dtrange", h4("Select a date range"), start='2014-11-12', end = '2015-08-21',
                          min='2014-11-12', max ='2015-08-21')),
    column(3, checkboxGroupInput("pages", label = h4("Select Events on One or More Pages"), 
                                 choices = list('Read Article Page' = 1,
                                                'Project Design Page' = 2,
                                                'Dashboard Page' = 3,
                                                'Project Editing Page' = 4,
                                                'Explore Page' = 5,
                                                'Read Project Page' = 7),
                                 selected = c(1,4,7))
           ),
    column(3, checkboxGroupInput("events", label = h4("Select One or More Event Types"), 
                                 choices = list('Adjust Preferences & Using Filters' = 1,
                                                'Writing/Editing' = 2,
                                                'Collecting Text & Pictures' = 3,
                                                'Browsing/Read Content/Enter Page' = 4,
                                                'Using Discussion Boards' = 5,
                                                'Using Text-To-Speech Support/Dictionary/Translation' = 6),
                                 selected = 1:6)
    ),
    column(3, checkboxGroupInput("iep", label = h4("Choose Students based on IEP Status"),
                                 choices = list('IEP (Individual Education Plan' = 0,
                                                'Non-IEP' = 1),
                                 selected = 0:1),
           checkboxGroupInput("ell", label = h4("Choose Students based on ELL Status"),
                              choices = list('ELL' = 1,
                                             'Non-ELL' = 0),
                              selected = 0:1)
    )
  )
  
  
  
  )

)
