library(shiny)
library(ggplot2)
library(plotly)
library(DT)
ui <- shinyUI(
  fluidPage(
    h1("Iris App"),
    sidebarLayout(
      sidebarPanel(
        h2("Sidebar"),
        ##############Can do this because iris is defined in the global environment########################
        #I don't want anything selected when I lauch the app...
        selectizeInput("xax", "x-axis", choices = c("",colnames(iris)[-5]), select = NULL),
        selectizeInput("yax", "y-axis", choices = c("",colnames(iris)[-5]), select = NULL),
        
        selectizeInput("filter", "Include Species: ", choices = unique(iris$Species), select = unique(iris$Species), multiple = TRUE),
        selectInput("colorBy", "Color By", choices = c("NULL", colnames(iris)[5])),
        checkboxInput("fit", "Fit Linear")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput("irisP", height = 800)),
          tabPanel("Data", dataTableOutput("irisT"))
        )
      )
    )
  )
)

server <- shinyServer(function(input, output){
  
  DATA <- reactive({
    iris[iris$Species %in% input$filter,]
  })
  
  output$irisP <- renderPlotly({
    # idata <- iris[iris$Species %in% input$filter,]
    idata <- DATA()
    #Before rendering these requirements must be met
    validate(
      need(input$xax != "" & input$yax != "", message = "Select x axis and y axis")
    )
    
    aes_point <- aes_string(x = input$xax, y = input$yax, color = input$colorBy)
    
    p <- ggplot(idata, aes_point) + geom_point()
    if(input$fit) p <- p + geom_smooth(method = "lm")
    p
  })
  
  output$irisT <- renderDataTable({
    # iris[iris$Species %in% input$filter,]
    DATA()
  })
  
  
})

shinyApp(ui = ui, server = server)
