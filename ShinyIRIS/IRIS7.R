library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(crosstalk)
ui <- shinyUI(
  fluidPage(
    h1("Iris App"),
    sidebarLayout(
      sidebarPanel(
        h2("Sidebar"),
        selectInput("data", "Select Data", c("iris", "anorexia", "boston", "browse...")),
        #Allows your widgets to be reactive
        uiOutput("filetype"),
        uiOutput("file"),
        uiOutput("xaxUI"),
        uiOutput("yaxUI"),
        uiOutput("colorByUI"),
        checkboxInput("fit", "Fit Linear")
      ),
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotlyOutput("dataP", height = 800)),
          tabPanel("Data", dataTableOutput("irisT"))
        )
      )
    )
  )
)

server <- shinyServer(function(input, output){
  

  DATA <- reactive({
    # if(input$data == "iris") data <- iris
    # if(input$data == "anorexia") data <- MASS::anorexia
    # if(input$data == "biopsy") data <- MASS::biopsy
    # if(input$data == "boston") data <- MASS::Boston
    # if(input$data == "traffic") data <- MASS::Traffic
    # if(input$data == "browse...") data <- input$f

    data <- switch(
      input$data,
      "iris" = iris,
      "anorexia" = MASS::anorexia,
      "biopsy" = MASS::biopsy,
      "boston" = MASS::Boston,
      "traffic" = MASS::Traffic,
      "browse..." = input$f
    )
    
    # DATA()[iris$Species %in% input$filter,]
    data
  })
  
  
  output$filetype <- renderUI({
    if(input$data == "browse...") radioButtons("ft", "File Type", choices = c(".csv", ".RDS"))
  })
  output$file <- renderUI({
    if(input$data == "browse...") fileInput("f", "File", multiple = FALSE, accept = input$ft)
  })
  
  #Checks the class of all your variables to assess which widget it can belong to
  DC <- reactive({
    sapply(DATA(), class)
  })
  
  output$xaxUI <- renderUI({
    # For all you "numeric" variables, they are allowed to be either your x-axis or y-axis
    choices <- colnames(DATA())[DC() == "numeric"]
    selectInput("xax", "x-axis", choices = c("",choices))
  })
  output$yaxUI <- renderUI({
    choices <- colnames(DATA())[DC() == "numeric"]
    selectInput("yax", "y-axis", choices = c("",choices))
  })
  
  output$colorByUI <- renderUI({
    # You are able to color by all of you categorical variables
    choices <- colnames(DATA())[DC() == "factor"]
    radioButtons("colorBy", "Color By", choices = c("NULL", choices))
  })

  
  
  
  output$dataP <- renderPlotly({
    validate(
      need(input$xax != "" & input$yax != "", message = "Select x axis and y axis")
    )
    validate(
      need(input$xax %in% colnames(DATA()) & input$yax %in% colnames(DATA()), message = "LOADING")
    )

    pdat <- DATA()
    
    aes_point <- aes_string(x = input$xax, y = input$yax, color = input$colorBy)
    
    p <- ggplot(pdat, aes_point) + geom_point()
    if(input$fit) p <- p + geom_smooth(method = "lm")
    p
  })
  
  output$irisT <- renderDataTable({
    DATA()
  })
  
})

shinyApp(ui = ui, server = server)
