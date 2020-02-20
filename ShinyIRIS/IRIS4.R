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
        selectInput("colorBy", "Color By", choices = c("NULL", colnames(iris)[5])),
        checkboxInput("fit", "Fit Linear")
      ),
      mainPanel(
        #Insert a set of tab panel
        tabsetPanel(
          #insert the individual tab
          tabPanel("Plot", plotlyOutput("irisP", height = 800)),
          #insert the individual tab
          tabPanel("Data", dataTableOutput("irisT"))
        )
      )
    )
  )
)

server <- shinyServer(function(input, output){
  output$irisP <- renderPlotly({
    aes_point <- aes_string(x = input$xax, y = input$yax, color = input$colorBy)
    
    p <- ggplot(iris, aes_point) + geom_point()
    if(input$fit) p <- p + geom_smooth(method = "lm")
    p
  })
  
  output$irisT <- renderDataTable({
    iris
  })
  
  
})

shinyApp(ui = ui, server = server)
