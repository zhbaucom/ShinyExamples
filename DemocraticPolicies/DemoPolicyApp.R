library(shiny)
library(DT)
library(knitr)

DemTabs <- readRDS("DemocraticNomineePolicies.RDS")
DemTabNames <- names(DemTabs)
DemNames <- as.character(DemTabs[[1]]$Candidate)
DemSR <- as.character(DemTabs[[1]]$`Still running `)
qbc <- readRDS("qbc.RDS")


ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h2("Select Filters"),
        uiOutput("tnui"),
        selectInput("Filters", "Select Filter", choices = c("Candidate", "Still Running?"), multiple = TRUE),
        uiOutput("cand"),
        uiOutput("srui")
      ),
      mainPanel(
        tabsetPanel( id = "tabs",
          tabPanel("By category", value = "t1",
            uiOutput("mainHead"),
            dataTableOutput("demT")
          ),
          tabPanel("One tab",value = "t2",
            tableOutput("ktdem")
          )
        )
        
      )
    )
  )
)

server <- shinyServer(function(input, output){
  output$mainHead <- renderUI({h2(input$TabNames)})
  
  output$tnui <- renderUI({
    if(input$tabs == "t2")TF <- TRUE else TF <- FALSE
    selectInput("TabNames", "Policy Subject", choices = DemTabNames, multiple = TF)
      
  })
  
  output$cand <- renderUI({
    if("Candidate" %in% input$Filters)
      selectInput("candidates", "Select Candidates", choices = DemNames, multiple = TRUE, selected = "")
  })
  
  output$srui <- renderUI({
    if("Still Running?" %in% input$Filters)
      radioButtons("stillRunning", "Still Running?", choices = c("Yes", "No"), selected = NULL)
  })
  
  output$demT <- renderDataTable({
    tab <- DemTabs[[input$TabNames]]
    if(input$Filters == "Candidate" && !is.null(input$candidates))
      tab <- tab[tab$Candidate %in% input$candidates,]
    if(input$Filters == "Still Running?" && !is.null(input$stillRunning))
      tab <- tab[tab$`Still running ` %in% input$stillRunning,]
    tab
  })
  
  output$ktdem <- function(){
    tab <- qbc
    if( "Candidate" %in% input$Filters && !is.null(input$candidates))
      tab <- tab[,c(1,2, which(colnames(tab) %in% input$candidates))]
    if("Still Running?" %in% input$Filters && !is.null(input$stillRunning))
      tab <- tab[,c(1,2, grep(input$stillRunning, tab[1,]))]
    if(!is.null(input$TabNames))
      tab <- tab[tab$Category %in% input$TabNames,]
    
    catSum <- cumsum(table(tab$Category))
    
    x <- tab %>%
      select(-Category) %>%
      kable(escape = F, align = "c", row.names = FALSE)%>%
      kable_styling(c("striped", "condensed"), full_width = F, fixed_thead = TRUE) %>%
      pack_rows(names(catSum)[1], 1, catSum[1])
    
    for(i in 2:length(catSum)){
      x <-  x %>%
        pack_rows(names(catSum)[i], catSum[i-1] + 1, catSum[i])
    }
    
    x
    
  }
  

})

shinyApp(ui = ui, server = server)