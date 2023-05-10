#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(foreign)
library(dplyr)
library(surveygraphr)

load("ICSMP_500.RData")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "SurveyGraph"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulated Data", tabName = "simulateddata", icon = icon("random", lib = "glyphicon")),
      
      menuItem("Uploaded Data",
               tabName = "uploadeddata",
               icon = icon("transfer", lib = "glyphicon")
      )
    )
  ),
  dashboardBody(
    tabItems(
      
      ##### Plot (First tab) #####
      tabItem(tabName = "simulateddata",
              fluidRow(
                
                box(verbatimTextOutput("code1"))
                ,
                box(
                  plotOutput("plot1")
                  
                )
              )
      ),
      ##### Raw Tweets (Second tab) #####
      tabItem(tabName = "uploadeddata",
              fluidRow(
                box(
                  width = 3,
                  
                  fileInput("file1", "Upload SPSS File",
                            multiple = TRUE,
                            accept = c(".sav")),tags$hr(),
                  
                  # create a dropdown menu for selecting the dataset to be used
                  selectInput("dataset","Data:",
                              choices =list(ICSMP_500 = "ICSMP_500",
                                            COVID_measures = "COVID_measures",
                                            uploaded_file = "inFile"), selected=NULL),
                  # create a dropdown menu for selecting variable 1
                  selectInput("v1","Variable 1", choices = NULL),
                  # create a dropdown menu for selecting variable 2
                  selectInput("v2","Variable 2", choices = NULL),
                  # create a dropdown menu for selecting variable 3
                  selectInput("v3","Variable 3", choices = NULL), #,
                  
                  # create a dropdown menu for selecting variable 3
                  selectInput("v4","Variable 4", choices = NULL), #,
                  
                  # create a dropdown menu for selecting variable 3
                  selectInput("v5","Variable 5", choices = NULL), #,
                  
                  numericInput("inNumber", "How many participants", 20)
                  
                  
                ),
                
                box(
                  plotOutput("plot2")
                  ,verbatimTextOutput("datahead")
                )
                # box(
                #   verbatimTextOutput("selected_variables"),
                #   DT::dataTableOutput("table1"))
              ),
              
             
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  observeEvent(input$file1,{
    inFile<<-upload_data()
  })
  
  #output$selected_variables <- renderPrint()
  
  
  #update variables based on the data
  observe({
    #browser()
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectInput(session, "v1", choices = var.opts)
    updateSelectInput(session, "v2", choices = var.opts)
    updateSelectInput(session, "v3", choices = var.opts)
    updateSelectInput(session, "v4", choices = var.opts)
    updateSelectInput(session, "v5", choices = var.opts)
  })
  
  make_a <- reactive({input$inNumber})
  # get data object
  get_data<-reactive({
    
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    obj<-list(data=get(input$dataset),
              v1=input$v1,
              v2=input$v2,
              v3=input$v3,
              v4=input$v4,
              v5=input$v5
    )
    
    #require all to be set to proceed
    if(any(sapply(obj,check))) return()
    #make sure choices had a chance to update
    check<-function(obj){
      !all(c(obj$dv,obj$iv,obj$mediator) %in% colnames(obj$data))
    }
    
    if(check(obj)) return()
    
    
    obj
  })
  
  upload_data<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    foreign::read.spss(input$file1$datapath, to.data.frame = TRUE)
    
  })
  
  output$datahead <- renderPrint({
    obj <- get_data()

    #S <- as.data.frame(unlist(obj$data))
    
    S <- obj$data
    
    # S <- cbind.data.frame(
    #   as.numeric(unlist(S[1]))
    #   ,as.numeric(unlist(S[2]))
    #   ,as.numeric(unlist(S[3]))
    # )
    # S
    
    
    c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)
    
    S <- S %>% select(c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5))
    S <- cbind.data.frame(
      as.numeric(unlist(S[1]))
      ,as.numeric(unlist(S[2]))
      ,as.numeric(unlist(S[3]))
      ,as.numeric(unlist(S[4]))
      ,as.numeric(unlist(S[5]))
    )
    colnames(S) <- c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)
    head(S)
  })
  
    output$plot2 <- renderPlot({
      
      obj <- get_data()
      
      S <- obj$data
      
      c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)
      
      S <- S %>% select(c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5))
      S <- cbind.data.frame(
        as.numeric(unlist(S[1]))
        ,as.numeric(unlist(S[2]))
        ,as.numeric(unlist(S[3]))
        ,as.numeric(unlist(S[4]))
        ,as.numeric(unlist(S[5]))
      )
      colnames(S) <- c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)
      #colnames(S) <- c(obj$v1,obj$v2,obj$v3)
      
      a1 <- make_a()
      S <- sample_n(S,a1)
      # as.data.frame(unlist(obj$data))
      # S <- obj$data
      # S <- cbind.data.frame(
      #   as.numeric(unlist(S[1]))
      #   ,as.numeric(unlist(S[2]))
      #   ,as.numeric(unlist(S[3]))
      # )
      #S <- as.numeric(S[1])
      edgelist <- surveygraphr::listgraph(S)
      edgelist
      g1 <- igraph::make_graph(edges=edgelist, directed=FALSE)
      
      
      plot(g1, vertex.size=5, vertex.label=NA)
      
    #   
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white',
    #          xlab = 'Waiting time to next eruption (in mins)',
    #          main = 'Histogram of waiting times')
    })
    
    output$plot1 <- renderPlot({
      S1 <- surveygraphr::gensurvey(200,25)
      results <- surveygraphr::exploregraph(S1)
      edgelist <- surveygraphr::listgraph(S1)
      g <- igraph::make_graph(edges=edgelist, directed=FALSE)
      plot(g, vertex.size=5, vertex.label=NA)
      
    })
    
    output$code1 <- renderPrint({
      print('S1 <- surveygraphr::gensurvey(200,25)')
      print('results <- surveygraphr::exploregraph(S1)')
      print('edgelist <- surveygraphr::listgraph(S1)')
      print('g <- igraph::make_graph(edges=edgelist, directed=FALSE)')
      print('plot(g, vertex.size=5, vertex.label=NA)')
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)