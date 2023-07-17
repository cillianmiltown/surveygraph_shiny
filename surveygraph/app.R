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
#devtools::install_github("surveygraph/surveygraphr")
library(surveygraph)
library(igraph)

load("ICSMP_500.RData")
load("ESS_500.RData")



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
                              min = 0, max = 1,
                              value = 0.5, step = 0.01),
                  sliderInput("threshold_sim", "Threshold:",
                              min = 0, max = 1,
                              value = 0.5, step = 0.001),
                  radioButtons("layer1", "Select Layer to display",
                               c("Agent" = "agent1",
                                 "Symbolic" = "symbolic1")),
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
                  
                  h4("Clean up plots"),
                  # checkboxInput("rm_iso1", "Remove Isolated Nodes (respondents)", FALSE
                  # ),
                  # checkboxInput("rm_iso2", "Remove Isolated Nodes (items)", FALSE
                  # ),
                  
                  
                  sliderInput("threshold_up", "Threshold:",
                              min = 0, max = 1,
                              value = 0.5, step = 0.001),
                  radioButtons("layer2", "Select Layer to display",
                               c("Agent" = "agent2",
                                 "Symbolic" = "symbolic2")),
                  
                  checkboxInput("ess_missing", "Remove ESS missing values (77, 88, 99)", FALSE
                  ),
                  tags$hr(), 
                  
                  # create a dropdown menu for selecting the dataset to be used
                  selectInput("dataset","Data:",
                              choices =list(
                                
                                COVID_measures = "COVID_measures",
                                ICSMP_500 = "ICSMP_500",
                                ESS_500 = "ESS_500",
                                ESS_GB_500 = "ESS_GB_500",
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
                  
                  # tags$hr(), 
                  # h4("Clean up plots"),
                  # checkboxInput("rm_iso1", "Remove Isolated Nodes (respondents)", FALSE
                  # ),
                  # checkboxInput("rm_iso2", "Remove Isolated Nodes (items)", FALSE
                  # ),
                  tags$hr(), 
                  numericInput("inNumber", "Number of participants for Plot", 100)
                  
                  
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
    var.opts<-colnames(get(input$dataset))
    #rm(d)
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
  
  
  get_input <- reactive({names(input)})
  make_a <- reactive({input$inNumber})
  get_d <- reactive({
    sort(
      setdiff(
        names(input),
        c("sidebarCollapsed", "inNumber", "sidebarItemExpanded"
          , "dataset", "rm","add", "file1"
          , "polarization"
         # ,"rm_iso1","rm_iso2"
          ,"ess_missing", "threshold_sim", "layer1","threshold_up","layer2")
      ))[c(0:(input_counter()+1))]
    
    
  })
  make_polarization <- reactive({input$polarization})
  make_threshold_sim <- reactive({input$threshold_sim})
  make_threshold_up <- reactive({input$threshold_up})
  
  # get data object
  get_data<-reactive({
    #rm(d)
    if(!exists(input$dataset)) return() # if no upload
    
    check<-function(x){is.null(x) || x==""}
    if(check(input$dataset)) return()
    
    polarization <- make_polarization()
    
    d <- get_d()
    # d <- setdiff(
    #   names(input),
    #   c("sidebarCollapsed", "inNumber", "sidebarItemExpanded", "dataset"
    #     , "rm","add", "file1", "polarization")
    # )
    
    
    
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
    # if(any(sapply(obj,check))) return()
    # #make sure choices had a chance to update
    # check<-function(obj){
    #   !all(c(obj$dv,obj$iv,obj$mediator) %in% colnames(obj$data))
    # }
    # 
    # if(check(obj)) return()
    # 
    
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
    # d <- setdiff(
    #   names(input),
    #   c("sidebarCollapsed", "inNumber", "sidebarItemExpanded", "dataset", "rm","add", "file1")
    # )
    d <- get_d()
    
    inputted_variables <- as.vector(unlist(obj[2:(length(d)+1)][1]))
    #rm(d)
    S <- S %>% select(inputted_variables)
    if (
      input$ess_missing==1
    ) 
    {
      df <- S
      val_repl <- c(77,88,99)
      val_repl 
      df_temp <- sapply(df,function(x) replace(x, x %in% val_repl, NA))
      df_temp <- as.data.frame(df_temp)
      sum(complete.cases(df_temp))
      S <- as.data.frame(df_temp[which(complete.cases(df_temp)),])
      colnames(df)
      S <- `colnames<-`(S, colnames(df))
      S
      
    } else {
      S
    }
    S_full <- S
    S[] <- lapply(S, as.numeric)
    a1 <- make_a()
    S <- sample_n(S,a1)
    # head(S)
    # obj[2]
    # get_d()
    cat(
      "Full dimenssions of selected data",
      paste(dim(S_full)[1], " Rows"),
      paste(dim(S_full)[2], " Columns"),
      "",
      "Variables Selected:",
      sep="\n")
    print(head(S))
    
  })
  
  #### output plot ####
  output$plot2 <- renderPlot({
    
    
    
    obj <- get_data()
    
    S <- obj$data
    d <- get_d()
    
    inputted_variables <- as.vector(unlist(obj[2:(length(d)+1)][1]))
    #rm(d)
    S <- S %>% select(inputted_variables)
    
    S[] <- lapply(S, as.numeric)
    
    
    if (
      input$ess_missing==1
    ) 
    {
      df <- S
      val_repl <- c(77,88,99)
      val_repl 
      df_temp <- sapply(df,function(x) replace(x, x %in% val_repl, NA))
      df_temp <- as.data.frame(df_temp)
      sum(complete.cases(df_temp))
      S <- as.data.frame(df_temp[which(complete.cases(df_temp)),])
      colnames(df)
      S <- `colnames<-`(S, colnames(df))
      S
      
    } else {
      S
    }
    
    a1 <- make_a()
    S <- sample_n(S,a1)
    
    S[] <- lapply(S, as.numeric)
    
    
    
    S <- na.omit(S)
    
    (S[,1])
    S1 <- S
    
    
    threshold_up <- make_threshold_up()
    
    if(input$layer2=="agent2")
    {
      #S1 <- surveygraph::make_synthetic_data(nrow=100, ncol=15, polarisation=polarization, minority =0.5)
      
      S1_agent <- make_projection(S1, #S1[,-1],
                                  layer = "agent", 
                                  threshold_method = "target_lcc", 
                                  method_value = threshold_up, 
                                  centre = FALSE)
      S1_agent_graph <- graph.data.frame(S1_agent , directed=FALSE)
      
      girvan_newman <- cluster_edge_betweenness(S1_agent_graph)
      
      #communities<-cluster_edge_betweenness(S1_agent_graph) 
      
      V(S1_agent_graph)$color <- membership(girvan_newman)
      
      plot(#communities,
        S1_agent_graph, vertex.size = 5, vertex.label = NA, main="respondents"
      )
    }
    
    if(input$layer2=="symbolic2")
    {
      S1_symbolic <- make_projection(S1, #S1[,-1],
                                     layer = "symbolic", 
                                     threshold_method = "target_lcc", 
                                     method_value = threshold_up, 
                                     centre = FALSE)
      
      S1_symbolic_graph <- graph.data.frame(S1_symbolic , directed=FALSE)
      
      
      
      
      plot(S1_symbolic_graph, vertex.size = 5, vertex.label = NA,main="items")
      
      
    }
    
  })
  
  #### Simulated Output ####
  
  output$plot1 <- renderPlot({
    
    #rm(list=ls())
    
    # S1 <- surveygraph::gensurvey(200,25)
    # results <- surveygraph::exploregraph(S1)
    # edgelist <- surveygraph::listgraph(S1)
    # g <- igraph::make_graph(edges=edgelist, directed=FALSE)
    # plot(g, vertex.size=5, vertex.label=NA)
    
    polarization <- make_polarization()
    threshold <- make_threshold_sim()
    S1 <- surveygraph::make_synthetic_data(nrow=100, ncol=15, polarisation=polarization, minority =0.5)
    
    if(input$layer1=="agent1")
    {
    S1_agent <- make_projection(S1[,-1],
                                layer = "agent", 
                                threshold_method = "target_lcc", 
                                method_value= threshold, 
                                centre = FALSE)
    S1_agent_graph <- graph.data.frame(S1_agent , directed=FALSE)
    
    girvan_newman <- cluster_edge_betweenness(S1_agent_graph)
    
    #communities<-cluster_edge_betweenness(S1_agent_graph) 
    
    V(S1_agent_graph)$color <- membership(girvan_newman)
    
    plot(#communities,
      S1_agent_graph, vertex.size = 5, vertex.label = NA, main="respondents"
    )
    }
    
    if(input$layer1=="symbolic1")
    {
    S1_symbolic <- make_projection(S1[,-1],
                                   layer = "symbolic", 
                                   threshold_method = "target_lcc", 
                                   method_value= threshold, 
                                   centre = FALSE)
    
    S1_symbolic_graph <- graph.data.frame(S1_symbolic , directed=FALSE)
    
    
    
    
    plot(S1_symbolic_graph, vertex.size = 5, vertex.label = NA,main="items")
    
    
    }
    
    
    # 
    # 
    # S1 <- surveygraph::make_synthetic_data(m=300, n=15, polarisation=polarization)
    # 
    # names1 <- data.frame(id=c(1:length(S1$X1)), group=S1$X1)
    # names2 <- data.frame(id=c(1:length(S1)))
    # 
    # edgelists <- surveygraph::graph_edgelists(S1)
    # 
    # g1 <- igraph::graph.data.frame(edgelists[[1]], vertices=names1, directed=FALSE)
    # g2 <- igraph::graph.data.frame(edgelists[[2]], vertices=names2, directed=FALSE)
    # 
    # V(g1)$color <- ifelse(V(g1)$group == 1, "blue", "red")
    # 
    # isolated_nodes1 <- which(degree(g1)==0)
    # isolated_nodes2 <- which(degree(g2)==0)
    # 
    # g1c <- delete.vertices(g1, isolated_nodes1)
    # g2c <- delete.vertices(g2, isolated_nodes2)
    # 
    # E(g2c)$label= E(g2c)$weight
    # 
    # par(mfrow=c(1,2), mar=c(1,1,1,1))
    # plot(g1c, vertex.size=2, vertex.label=NA, edge.width=0.2, layout=layout.fruchterman.reingold, main="respondents")
    # plot(g2c, vertex.size=10, edge.width=1.0, layout=layout.fruchterman.reingold, main="items")
    # 
    
    
  })
  
  output$code1 <- renderPrint({
    
    
    
    if(input$layer1=="agent1")
    {
      
      cat(
        
        'S1 <- surveygraph::make_synthetic_data(nrow=100, ncol=15, polarisation=polarization, minority =0.5)',
        
        'S1_agent <- make_projection(S1[,-1],',
        '                            layer = "agent", ',
        '                            threshold_method = "target_lcc", ',
        '                            method_value= threshold, ',
        '                            centre = FALSE)',
        'S1_agent_graph <- graph.data.frame(S1_agent , directed=FALSE)',
        
        'girvan_newman <- cluster_edge_betweenness(S1_agent_graph)',
        
        'V(S1_agent_graph)$color <- membership(girvan_newman)',
        
        'S1_agent_graph, vertex.size = 5, vertex.label = NA, main="respondents")',
        
        sep="\n"
      )
      
      
      
      
    }
    
    if(input$layer1=="symbolic1")
    {
      
      cat(
        
        'S1 <- surveygraph::make_synthetic_data(nrow=100, ncol=15, polarisation=polarization, minority =0.5)',
        
        'S1_symbolic <- make_projection(S1[,-1],',
        '                            layer = "symbolic", ',
        '                            threshold_method = "target_lcc", ',
        '                            method_value= threshold, ',
        '                            centre = FALSE)',
        'S1_symbolic_graph <- graph.data.frame(S1_agent , directed=FALSE)',
        
        'girvan_newman <- cluster_edge_betweenness(S1_symbolic_graph)',
        
        'V(S1_symbolic_graph)$color <- membership(girvan_newman)',
        
        'plot(S1_symbolic_graph, vertex.size = 5, vertex.label = NA,main="items")',
        
        sep="\n"
      )
      
      
      
    }
    
    
    
    
  })
  
  observe(print(get_d()))
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# Possible solution for deleting redundant values in list at
# https://github.com/rstudio/shiny/issues/2374
