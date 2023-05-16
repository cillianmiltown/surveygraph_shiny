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
library(igraph)

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
      
      ##### Simulated Data (First tab) #####
      tabItem(tabName = "simulateddata",
              fluidRow(
                
                box(
                  sliderInput("polarization", "Polarization:",
                              min = 0, max = 4,
                              value = 0.5, step = 0.01),
                  verbatimTextOutput("code1"))
                ,
                box(
                  plotOutput("plot1")
                  
                )
              )
      ),
      ##### Uploaded (SPSS) Data (Second tab) #####
      tabItem(tabName = "uploadeddata",
              fluidRow(
                box(
                  width = 3,
                  
                  fileInput("file1", "Upload SPSS File",
                            multiple = TRUE,
                            accept = c(".sav")),tags$hr(),
                  
                  # create a dropdown menu for selecting the dataset to be used
                  selectInput("dataset","Data:",
                              choices =list(
                                            COVID_measures = "COVID_measures",
                                            ICSMP_500 = "ICSMP_500",
                                            uploaded_file = "inFile"), selected=NULL),
                  # create a dropdown menu for selecting variable 1
                  
                  
                  
                  selectizeInput("v1", "Variable 1", choices = NULL),
                  # # create a dropdown menu for selecting variable 2
                  # selectInput("v2","Variable 2", choices = NULL),
                  # # create a dropdown menu for selecting variable 3
                  # selectInput("v3","Variable 3", choices = NULL), #,
                  # 
                  # # create a dropdown menu for selecting variable 3
                  # selectInput("v4","Variable 4", choices = NULL), #,
                  # 
                  # # create a dropdown menu for selecting variable 3
                  # selectInput("v5","Variable 5", choices = NULL), #,
                  
                  
                  actionButton(inputId = "rm", label = "-"),
                  actionButton(inputId = "add", label = "+"),
                  h4("Add/remove variables"),
                  h5("(Removing doesn't work properly yet. Refresh App instead)"),
                  
                  tags$hr(), 
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
  
  input_counter <- reactiveVal(0)
  
  observeEvent(input$add, {
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    input_counter(input_counter() + 1)
    insertUI(
      selector = "#rm", where = "beforeBegin",
      ui = div(id = paste0("selectize_div", input_counter())
               , selectizeInput(
                 paste0("v", (input_counter())+1)
                 , label = paste("Variable ", (input_counter()+1))
                 , choices = var.opts))
    )
  })
  observeEvent(input$rm, {
    removeUI(
      selector = paste0("#selectize_div", input_counter())
    )
    input_counter(input_counter() - 1)
  })
  
  observe({
    print(names(input))
    #print(input$a1)
    print(
      setdiff(
        names(input),
        c("sidebarCollapsed", "inNumber", "sidebarItemExpanded", "dataset", "rm","add", "file1")
      )
    )
  })
  
  
  #output$selected_variables <- renderPrint()
  
  
  #update variables based on the data
  observe({
    #browser()
    if(!exists(input$dataset)) return() #make sure upload exists
    var.opts<-colnames(get(input$dataset))
    updateSelectizeInput(session, "v1", choices = var.opts)
    # if(is.null(input$v2)==FALSE) {updateSelectizeInput(session, "v2", choices = var.opts)}
    # if(is.null(input$v3)==FALSE) {updateSelectizeInput(session, "v3", choices = var.opts)}
    # if(is.null(input$v4)==FALSE) {updateSelectizeInput(session, "v4", choices = var.opts)}
    # if(is.null(input$v5)==FALSE) {updateSelectizeInput(session, "v5", choices = var.opts)}
    
  })
  
  
  
  make_a <- reactive({input$inNumber})
  make_polarization <- reactive({input$polarization})
  # get data object
  get_data<-reactive({
    #rm(d)
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    polarization <- make_polarization()
    
    d <- setdiff(
      names(input),
      c("sidebarCollapsed", "inNumber", "sidebarItemExpanded", "dataset"
        , "rm","add", "file1", "polarization")
    )
    
    
    obj_vx_input_fun <- function(x){
      noquote(paste(noquote(d[x]),"=", "input$", noquote(d[x]),sep = ""))
      }
    
    inputted_vector <- sapply(1:length(d), obj_vx_input_fun)
    
    inputted_variables_fun <- function(x){
      eval(parse(text=inputted_vector[x]))}
    
    
    obj<-list(data=get(input$dataset),
              
              # setdiff(
              #   names(input),
              #   c("sidebarCollapsed", "inNumber", "sidebarItemExpanded", "dataset", "rm","add", "file1")
              # )

              
              
              sapply(1:length(d), inputted_variables_fun)
              # if(is.null(input$v1)==FALSE){v1=input$v1},
              # if(is.null(input$v2)==FALSE){v2=input$v2},
              # if(is.null(input$v3)==FALSE){v3=input$v3},
              # if(is.null(input$v4)==FALSE){v4=input$v4},
              # if(is.null(input$v5)==FALSE){v5=input$v5}
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
  
  get_data2 <- reactive({
    
    obj <- list(data = get(input$dataset))
    obj
  })
  
  upload_data<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    #could also store in a reactiveValues
    foreign::read.spss(input$file1$datapath, to.data.frame = TRUE)
    
  })
  
  #### output head ####
  output$datahead <- renderPrint({
    obj <- get_data()
    S <- obj$data
    d <- setdiff(
      names(input),
      c("sidebarCollapsed", "inNumber", "sidebarItemExpanded", "dataset", "rm","add", "file1")
    )
    inputted_variables <- as.vector(unlist(obj[2:(length(d)+1)][1]))
    S <- S %>% select(inputted_variables)
    
    S[] <- lapply(S, as.numeric)
    a1 <- make_a()
    S <- sample_n(S,a1)
    head(S)
    
  })
  
  #### output plot ####
    output$plot2 <- renderPlot({
      
      
      
      obj <- get_data()
      
      S <- obj$data
      
      d <- setdiff(
        names(input),
        c("sidebarCollapsed", "inNumber", "sidebarItemExpanded", "dataset", "rm","add", "file1")
      )
      inputted_variables <- as.vector(unlist(obj[2:(length(d)+1)][1]))
      S <- S %>% select(inputted_variables)
      
      S[] <- lapply(S, as.numeric)
      # c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)
      # 
      # S <- S %>% select(c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5))
      # S <- cbind.data.frame(
      #   as.numeric(unlist(S[1]))
      #   ,as.numeric(unlist(S[2]))
      #   ,as.numeric(unlist(S[3]))
      #   ,as.numeric(unlist(S[4]))
      #   ,as.numeric(unlist(S[5]))
      # )
      # colnames(S) <- c(obj$v1,obj$v2,obj$v3,obj$v4,obj$v5)
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
      S[] <- lapply(S, as.numeric)
      
      S <- na.omit(S)
      
      (S[,1])
      
      names1 <- data.frame(id=c(1:length(S[,1])), group=S[,1])
      names2 <- data.frame(id=c(1:length(S)))
      
      edgelists <- surveygraphr::graph_edgelists(S)
      
      g1 <- igraph::graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)
      g2 <- igraph::graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)
      
      
      V(g1)$group
      V(g1)$color <- ifelse(V(g1)$group == 10, "blue", "red")
      V(g1)$color
      
      isolated_nodes1 <- which(degree(g1)==0)
      isolated_nodes2 <- which(degree(g2)==0)
      
      g1c <- delete.vertices(g1, isolated_nodes1)
      g2c <- delete.vertices(g2, isolated_nodes2)
      
      E(g2c)$label= E(g2c)$weight
      
      par(mfrow=c(1,2), mar=c(1,1,1,1))
      plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2
           #, layout=layout.fruchterman.reingold
           , main="respondents")
      plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="items")
      # g1 <- igraph::make_graph(edges=edgelist, directed=FALSE)
      # 
      # 
      # plot(g1, vertex.size=5, vertex.label=NA)
      # 
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
    
  #### Simulated Output ####
  
    output$plot1 <- renderPlot({
      
      #rm(list=ls())
      
      # S1 <- surveygraphr::gensurvey(200,25)
      # results <- surveygraphr::exploregraph(S1)
      # edgelist <- surveygraphr::listgraph(S1)
      # g <- igraph::make_graph(edges=edgelist, directed=FALSE)
      # plot(g, vertex.size=5, vertex.label=NA)
      
      polarization <- make_polarization()
      
      S1 <- surveygraphr::generate_survey_polarised(m=300, n=15, polarisation=polarization)
      
      names1 <- data.frame(id=c(1:length(S1$X1)), group=S1$X1)
      names2 <- data.frame(id=c(1:length(S1)))
      
      edgelists <- surveygraphr::graph_edgelists(S1)
      
      g1 <- igraph::graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)
      g2 <- igraph::graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)
      
      V(g1)$color <- ifelse(V(g1)$group == 1, "blue", "red")
      
      isolated_nodes1 <- which(degree(g1)==0)
      isolated_nodes2 <- which(degree(g2)==0)
      
      g1c <- delete.vertices(g1, isolated_nodes1)
      g2c <- delete.vertices(g2, isolated_nodes2)
      
      E(g2c)$label= E(g2c)$weight
      
      par(mfrow=c(1,2), mar=c(1,1,1,1))
      plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="respondents")
      plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="items")
      
      
      
    })
    
    output$code1 <- renderPrint({
      cat(
      "S1 <- surveygraphr::generate_survey_polarised(m=300, n=15, polarisation=.5)",
      "",
      "names1 <- data.frame(id=c(1:length(S1$X1)), group=S1$X1)",
      "names2 <- data.frame(id=c(1:length(S1)))",
      "",
      "edgelists <- surveygraphr::graph_edgelists(S1)",
      "",
      "g1 <- igraph::graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)",
      "g2 <- igraph::graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)",
      "",
      "V(g1)$color <- ifelse(V(g1)$group == 1, 'blue', 'red')",
      "",
      "isolated_nodes1 <- which(degree(g1)==0)",
      "isolated_nodes2 <- which(degree(g2)==0)",
      "",
      "g1c <- delete.vertices(g1, isolated_nodes1)",
      "g2c <- delete.vertices(g2, isolated_nodes2)",
      "",
      "E(g2c)$label= E(g2c)$weight",
      "",
      "par(mfrow=c(1,2), mar=c(1,1,1,1))",
      "plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main='respondents'')",
      "plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main='items')",
      sep="\n"
      )
    })
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
