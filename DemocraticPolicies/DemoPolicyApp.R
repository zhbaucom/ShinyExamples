library(shiny)
library(DT)
library(knitr)
library(tidyverse)
library(kableExtra)

DemTabs <- readRDS("data/DemocraticNomineePolicies.RDS")
DemTabNames <- names(DemTabs)
DemNames <- as.character(DemTabs[[1]]$Candidate)
DemSR <- as.character(DemTabs[[1]]$`Still running `)
qbc <- readRDS("data/qbc.RDS")
rQ <- readRDS("data/rQ.RDS")
tqbcLM <- readRDS("data/tqbcLM.RDS")
source("functions/ColorFun.R")



ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        uiOutput("ui1")
      ),
      mainPanel(
        tabsetPanel( id = "tabs",
          tabPanel("The Quiz", value = "Quiz",
                   textInput("User", h2("Name:")),
                   rQ, 
                   actionButton("Submit", "Submit")),           
          tabPanel("One tab",value = "t2",
            verbatimTextOutput("test"),
            tableOutput("ktdem") 
            
          )
        )
        
      )
    )
  )
)

server <- shinyServer(function(input, output){
  
  
  output$ui1 <- renderUI({
    
    if(input$tabs == "t2"){
      list(
        h2("Select Filters"),
        selectInput("TabNames", "Policy Subject", choices = DemTabNames, multiple = TRUE),
        selectInput("candidates", "Select Candidates", choices = DemNames, multiple = TRUE, selected = ""),
        radioButtons("stillRunning", "Still Running?", choices = c("All","Yes", "No"), selected = NULL),
        br(),
        h3("K-Mediods"),
        numericInput("k", "Number of Clusters", min = 1, max = 10, value = 2),
        actionButton("Execute", "Execute")
      )
    }else if(input$tabs == "Quiz"){
      h2("Complete the Survey")
    }

  })
  
  
  
  
  output$mainHead <- renderUI({h2(input$TabNames)})
  
  


  

  

  DATALIST <- reactive({
    ###GRABBING ALL USER INPUT
    userChar <- character((nrow(qbc)-1))
    for(i in 1:(nrow(qbc)-1)){
      if(!is.null(input[[paste("rQ", i, sep = "")]])){
        userChar[i] <- input[[paste("rQ", i, sep = "")]]
      }else{
        userChar[i] <- "?"
      }
    }
    
    userChar <- c("Yes", userChar)
    
    tab <- qbc %>%
      add_column(User = userChar, .after = 2) %>%
      mutate_at(vars(User), function(x){
        cell_spec(x, background = colorFun(as.character(x)))
      })
      

    
    
    if(!is.null(input$candidates))
      tab <- tab[,c(1,2, 3, which(colnames(tab) %in% input$candidates))]
    if(input$stillRunning == "No"){
      tab <- tab[,c(1,2, 3, grep(input$stillRunning, tab[1,]))]
    }else if(input$stillRunning == "Yes"){
      tab <- tab[,c(1,2, grep(input$stillRunning, tab[1,]))]
    }
      
    
    if(!is.null(input$TabNames))
      tab <- tab[tab$Category %in% input$TabNames,]
    
    tqbcLM <- tqbcLM[row.names(tqbcLM) %in% colnames(tab),colnames(tqbcLM) %in% tab$Topic]
    

    
    #KEEPING CHARACTER AND NUMERICAL INPUT
    
    userNum <- ifelse(userChar == "?" | userChar == "Unsure", 0, ifelse(
      userChar == "Yes", 1, -1
    ))
    
    
    
    outList <- list(tab=tab, tqbcLM=tqbcLM, userNum = userNum, userChar = userChar)
    outList
    
  })
  
  ###CREATING METHOD FOR GETTING DATA BASED ON REACTIVE BUTTON
  v <- reactiveValues(DoClust = FALSE)
  
  #IF THE INPUTS ARE CHANGED THEN UNDO CLUSTERING
  tolisten <- reactive({
    daList <- list(input$TabNames, input$Filters, input$candidates, input$stillRunning, input$k)
    daList
  })
  observeEvent(tolisten(), {
    v$DoClust <- FALSE
  })
  
  
  #IF BUTTON IS PUSHED DO CLUSTERING
  observeEvent(input$Execute, {
    v$DoClust <- TRUE
  })
  
  
  #IF BUTTON IS PUSHED THEN CLUSTERING WILL BE DONE HERE
  CLUSTDATA <- reactive({
    
    if(v$DoClust){
      x <- DATALIST()$tqbcLM
      s <- cluster::pam(x, input$k, FALSE)
      


      smed <- t(s$medoids)
      colnames(smed) <- paste("mediod:", 1:ncol(smed))
      smedYN <- apply(smed, 2, function(x){
        ifelse(x == 1, "Yes", ifelse(x == -1, "No", "Unclear"))
      })
      
      scols <- data.frame(smedYN) %>%
        rownames_to_column(var = "Topic")%>%
        mutate_at(vars(-Topic),function(x){
          cell_spec(x, background = colorFun(as.character(x)))
        })
      
      #User Answers
      UserCluster <- unname(which.min(apply(smed, 2, function(x){sum((x-DATALIST()$userNum)^2)})))
      
      
      sc <- c("User" = UserCluster, s$clustering)
      ccols <- sample(palette(), input$k)
      
      ccolsSC <- sapply(sc, function(x){ccols[x]})
      
      out <- list(scols = scols, ccolsSC = ccolsSC)
      
    }else{out <- NULL}
    out


  })
  

  
  output$ktdem <- function(){
    validate(
      need("candidates" %in% names(input), message = "Loading...")
    )
    tab <- DATALIST()$tab
    
    if(input$User != "") colnames(tab)[3] <- input$User
    
    tc <- table(tab$Category)
    
    catSum <- cumsum(tc[tc>0])
    
    
    
    #Column names of the table for clustering coloring
    CnamesTab <- colnames(tab %>% select(-Category))
    
    
    ###Closest candidate
    minCan <- apply(DATALIST()$tqbcLM, 1, function(x) sum((x - DATALIST()$userNum)^2))

    #column of closest candidate
    cc <- which(colnames(tab) %in% names(minCan)[which.min(minCan)]) 
    
    colnames(tab)[c(3, cc)] <- cell_spec(colnames(tab)[c(3, cc)], color = "red")
    

    x <- tab %>%
      select(-Category) %>%
      kable(escape = F, align = "c", row.names = FALSE)%>%
      kable_styling(c("striped", "condensed"), full_width = F, fixed_thead = TRUE) 
    
    

    if(!is.null(CLUSTDATA())){
      ccolsSC <- CLUSTDATA()$ccolsSC
      if(input$User != "") names(ccolsSC)[1] <- input$User
      for(i in unique(ccolsSC)){
        names1 <- ccolsSC[ccolsSC %in% i]
        cn1 <- which(CnamesTab %in% names(names1))
        x <- x %>%
          column_spec(cn1, background = i)
      }
    }
    
    x <- x %>%
      pack_rows(names(catSum)[1], 1, catSum[1])
    
    if(length(catSum) > 1){
      for(i in 2:length(catSum)){
        x <-  x %>%
          pack_rows(names(catSum)[i], catSum[i-1] + 1, catSum[i])
      }
    }
  
    x
  }
  

})

shinyApp(ui = ui, server = server)