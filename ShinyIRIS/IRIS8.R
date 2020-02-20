library(shiny)
library(ggplot2)
library(plotly)
library(DT)
library(crosstalk)
library(dplyr)
Iris2 <- iris
Iris2$id <- row.names(Iris2)

ui <- shinyUI(
  fluidPage(
    h1("Iris App"),
    sidebarLayout(
      sidebarPanel(
        h2("Sidebar"),
        ##############Can do this because iris is defined in the global environment########################
        selectInput("xax", "x-axis", choices = c("",colnames(Iris2)[-c(5,6)])),
        selectInput("yax", "y-axis", choices = c("",colnames(Iris2)[-c(5,6)])),
        selectizeInput("filter", "Include Species: ", choices = unique(Iris2$Species), select = unique(Iris2$Species), multiple = TRUE),
        selectInput("colorBy", "Color By", choices = c("NULL", colnames(Iris2)[5])),
        checkboxInput("fit", "Fit Linear")
      ),
      mainPanel(
        plotlyOutput("irisP", height = 500),
        DT::dataTableOutput("irisT")
      )
    )
  )
)


server <- shinyServer(function(input, output){
  #Reactive Data
  DATA <- reactive({
    Iris2[Iris2$Species %in% input$filter,]
  })
  #Share data between multiple "renders" designating the common id
  nd <- reactive({
    SharedData$new(DATA(), ~id)
  })
  
  
  #Switched to plotly because it was easier for the crosstalk
  output$irisP <- renderPlotly({
    validate(
      need(input$xax != "" & input$yax != "", message = "Select x axis and y axis")
    )
    idata <- DATA()
    
    #the cross talk object "nd" was placed in the irisT data.table below and it automatically creates the _row_select that outputs the ids selected in the DT
    s <- input$irisT_rows_selected 
    
    #Specify x-axis and y-axis label
    x <- list(
      title = input$xax
    )
    y <- list(
      title = input$yax
    )
    #If nothing is selected in irisT
    if (!length(s)) {
      #Not as moldable as ggplot. This specifies if we use the colorBy selection
      if(input$colorBy == "NULL"){
        pp <- nd() %>%
          plot_ly(x = ~get(input$xax), y = ~get(input$yax), mode = "markers", color = I("black"), name = 'Unfiltered') %>%
          layout(showlegend = T) %>% 
          #Indicates we can select points in the plotly and then we can transfer to the DT
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
      }else{
        pp <- nd() %>%
          plot_ly(x = ~get(input$xax), y = ~get(input$yax), mode = "markers", color = ~get(input$colorBy), name = ~get(input$colorBy)) %>%
          layout(showlegend = T) %>% 
          highlight("plotly_selected", color = I('red'), selected = attrs_selected(name = 'Filtered'))
      }
      #If something in DT was selected
    } else if (length(s)) {
      if(input$colorBy == "NULL"){
        pp <- idata %>%
          plot_ly() %>% 
          add_trace(x = ~get(input$xax), y = ~get(input$yax), mode = "markers", color = I('black'), name = 'Unfiltered') %>%
          layout(showlegend = T)
      }else{
        pp <- idata %>%
          plot_ly() %>% 
          add_trace(x = ~get(input$xax), y = ~get(input$yax), mode = "markers", color = ~get(input$colorBy), name = ~get(input$colorBy)) %>%
          layout(showlegend = T)
      }
      
      
      #This adds a red point for each row selected in the DT
      pp <- add_trace(pp, data = idata[idata$id %in% s, , drop = F], x = ~get(input$xax), y = ~get(input$yax), mode = "markers",
                      color = I('red'), name = 'Filtered')
    }
    
    #Adds the LM fit. and it's broken...
    if(input$fit){
      fit <- lm(formula(paste(input$yax, input$xax, sep = "~")), data = idata)
      fit.x <- idata[, input$xax]
      pp <- pp %>% 
        add_trace(x = fit.x, y = fitted(fit), color = I("blue"), mode = "lines")
      
    }
    
    pp %>%
      layout(xaxis = x, yaxis = y)
  })
  
  
  output$irisT <- DT::renderDataTable({
    #Assigns the reactive data
    idata <- DATA()
    #nd()$selection() is a vector of TRUE FALSE depending on if the observation id was selected in the plotly 
    idata$tf <- nd()$selection()
    
    #If nothing is selected from the plotly
    if (NROW(idata[nd()$selection(),]) == 0) {
      DT::datatable(idata, options = list(
        columnDefs = list(list(targets = 6, visible = FALSE))
      )) 
      #if something is selected from the plotly
    } else {
      #If something is selected in plotly I want it at the top of the DT 
      idata <- rbind(idata[idata$tf,],idata[!idata$tf,])
      DT::datatable(idata, options = list(
        columnDefs = list(list(targets = c(6, 7), visible = FALSE))
      )) %>%
        #Color the plotly selected rows as red
        DT::formatStyle("tf", target = "row",
                        backgroundColor = DT::styleEqual(c(0,1), c("white", "red")))
    }
    
  }, server = FALSE)
  
  
})


shinyApp(ui = ui, server = server)

