library(shiny)
library(DT)

DemTabs <- readRDS("DemocraticNomineePolicies.RDS")
DemTabNames <- names(DemTabs)
DemNames <- as.character(DemTabs[[1]]$Candidate)
DemSR <- as.character(DemTabs[[1]]$`Still running `)


ui <- shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h2("Select Filters"),
        selectInput("TabNames", "Policy Subject", choices = DemTabNames),
        selectInput("Filters", "Select Filter", choices = c("Candidate", "Still Running?"), multiple = TRUE),
        uiOutput("cand"),
        uiOutput("srui")
      ),
      mainPanel(
        uiOutput("mainHead"),
        dataTableOutput("demT")
      )
    )
  )
)

server <- shinyServer(function(input, output){
  output$mainHead <- renderUI({h2(input$TabNames)})
  
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
})

shinyApp(ui = ui, server = server)