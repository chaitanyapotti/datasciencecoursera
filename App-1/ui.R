library(shiny)
library(markdown)
library(ggplot2)
library(shinysky)
# Define UI for application that draws a histogram
shinyUI(navbarPage("Data Analytics",
  tabPanel("upload",tags$head(tags$style("tfoot {display: table-header-group;}")), sidebarLayout(position="right",
                sidebarPanel(
                  fileInput("file1", multiple = T, label = h3("Choose CSV or XLS file"), accept = c('text/csv','.csv','.xls','.xlsx')),
                  tags$hr(),
                  checkboxInput('header','Header',TRUE)
#                   radioButtons('sep', 'Separator',
#                                c(Comma=',',
#                                  Semicolon=';',
#                                  Tab='\t'),
#                                ','),
#                   radioButtons('quote', 'Quote',
#                                c(None='',
#                                  'Double Quote'='"',
#                                  'Single Quote'="'"),
#                                '"')
                ),
                mainPanel(
                  dataTableOutput('contents')
                )
  )),
  tabPanel("Summary",
           titlePanel("Visual Summaries"),
           verbatimTextOutput("summary"),
           verbatimTextOutput("headerr")
           ),
  tabPanel("Plots",
           sidebarLayout(position="right",
             sidebarPanel(
               selectInput('x', label = 'Xinput',choices =NULL),
               selectInput('y', label = 'Yinput', choices=NULL),
               radioButtons("plotType", "Plot Type", c("Scatter"="p","Line"="l","histogram"="h")),
               # submitButton("Submit")
               actionButton("actButton","Update Plot")
               
             ),
             mainPanel(
               plotOutput("plot1", click = "plot1_click",brush = brushOpts(id = "plot1_brush")),
#                verbatimTextOutput("click_info"),
#                verbatimTextOutput("brush_info"),
               verbatimTextOutput("sumLm1")
             )
           )
           
           ),
tabPanel("LinearRegression",
         sidebarLayout(position="right",
           sidebarPanel(
             #selectizeInput('x2', label = 'X2input',choices =NULL, multiple = T),
             selectInput('y2', label = 'Y2input', choices=NULL),
             #selectInput('Fit',label='FitType', choices =c()),
             actionButton("actButton2","Generate Model")
           ),
           mainPanel(
             verbatimTextOutput("sumLM")
           )
         )
),
tabPanel("DataEditing",
         sidebarLayout(position="right",
                       sidebarPanel(
         h4('Edit the table here'),
         h6('Click on a cell to edit')
         ),
         mainPanel(
           hotable("editTable")
         )
         )
         ),
tabPanel("NLQ",
         sidebarLayout(position="right",
                       sidebarPanel(
         
         ),
         mainPanel(
           textInput("NLQ",label=h3("Search"), value = NULL),
           actionButton("runNLQ","Go"),
           verbatimTextOutput("textNLQ"),
           plotOutput("plotNLQ")
         )))

))