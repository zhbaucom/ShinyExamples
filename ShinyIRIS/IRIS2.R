library(shiny)
library(ggplot2)
ui <- shinyUI(
  fluidPage(
    h1("Iris App"),
    sidebarLayout(
      sidebarPanel(
        h2("Sidebar"),
        ##############Can do this because iris is defined in the global environment########################
        radioButtons(inputId = "xax", "x-axis", choices = colnames(iris)[-5]),
        radioButtons(inputId = "yax", "y-axis", choices = colnames(iris)[-5]),
        #Color the points depending on 'Species'
        selectInput(inputId = "colorBy", "Color By", choices = c("NULL", "Species")),
        #Fit a linear model to the data
        checkboxInput(inputId = "fit", "Fit Linear")
      ),
      mainPanel(
        h1("Main Panel"),
        plotOutput("irisP", height = 400),
        tableOutput("irisT")
      )
    )
  )
)

server <- shinyServer(function(input, output){
  
  output$irisP <- renderPlot({
    
    #aes_string allows you to character in the aes statement unlike the regular aes.
    aes_point <- aes_string(x = input$xax, y = input$yax, color = input$colorBy)
    
    p <- ggplot(iris, aes_point) + geom_point()
    if(input$fit) p <- p + geom_smooth(method = "lm")
    p
  })
  output$irisT <- renderTable({
    iris
  })
})

shinyApp(ui = ui, server = server)
