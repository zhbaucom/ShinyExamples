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
        radioButtons("xax", "x-axis", choices = colnames(iris)[-5]),
        radioButtons("yax", "y-axis", choices = colnames(iris)[-5]),
        #I want to filter the data based on selected species
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
  
  #Creates a reactive object you can share within render statements
  DATA <- reactive({
    iris[iris$Species %in% input$filter,]
  })
  
  output$irisP <- renderPlotly({
    # idata <- iris[iris$Species %in% input$filter,]
    idata <- DATA()
    aes_point <- aes_string(x = input$xax, y = input$yax, color = input$colorBy)
    
    p <- ggplot(idata, aes_point) + geom_point()
    if(input$fit) p <- p + geom_smooth(method = "lm")
    p
  })
  
  output$irisT <- renderDataTable({
    iris[iris$Species %in% input$filter,]
    DATA()
  })
  
  
})

shinyApp(ui = ui, server = server)
