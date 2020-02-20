library(shiny)

ui <- shinyUI(
  fluidPage(
    h1("Iris App"),
    sidebarLayout(
      sidebarPanel(
        h2("Sidebar"),
        selectInput(inputId = "xax", label = "x-axis", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")),
        selectInput(inputId = "yax", label = "y-axis", choices = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))
        ##############Can do this because iris is defined in the global environment########################
        # selectInput("xax", "x-axis", choices = colnames(iris)[-5]),
        # selectInput("yax", "y-axis", choices = colnames(iris)[-5])
      ),
      mainPanel(
        h2("Main Panel"),
        plotOutput("irisP", height = 800),
        tableOutput("irisT")
      )
    )
  )
)

server <- shinyServer(function(input, output){
  

  output$irisP <- renderPlot({
    # Can do any computation in a render as long as your printed output is the same as the render type
    a <- 2*3
    b <- 5*a
    plot(iris[,input$xax], iris[,input$yax], xlab = input$xax, ylab = input$yax)
  })
  
  output$irisT <- renderTable({
    iris
  })
  
})

shinyApp(ui = ui, server = server)

