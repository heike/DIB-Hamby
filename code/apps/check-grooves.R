library(shiny)
library(shinydashboard)
library(rgl)
#options(rgl.useNULL=FALSE)
library(shinycssloaders)
library(manipulateWidget)
library(crosstalk)
#remotes::install_github("rstudio/webshot2")
library(webshot2)

#library(shinyRGL) # remotes::install_github("trestletech/shinyRGL")
#library(rglwidget)
library(plotly)
library(ggplot2)
library(tidyr)
library(dplyr)
library(readr)
library(x3ptools)

ui = fluidPage(
  selectInput("k","Investigate kth plot:", selected = 1,
              choices=1:length(grooves)),
  textOutput("groovelocations"),
  actionButton("confirm", "Confirm"),
  actionButton("save", "Save"),
  plotOutput("groovePlot", click = "plot_click"),
  verbatimTextOutput("info")
)

server = function(input, output, session) {
  output$groovePlot <- renderPlot({
    k <- as.numeric(input$k)
    p <- grooves[[k]]$plot

    p
  })
  output$groovelocations <- renderText({
    paste("Left Groove: ",grooves[[as.numeric(input$k)]]$groove[1],
          " Right Groove: ",grooves[[as.numeric(input$k)]]$groove[2])
  })
  observeEvent(input$confirm,{
    cat(str(input$k))
    updateSelectInput(session, "k","Investigate kth plot:",
                      selected = as.numeric(input$k)+1,
                      choices=1:length(grooves))
  })
  observeEvent(input$save,{
    saveRDS(grooves, file="data/grooves.rda")
    cat("groove data saved\n")
  })

  observeEvent(input$plot_click,{
    k <- as.numeric(input$k)
    xloc <- input$plot_click$x

    gr <- grooves[[k]]$groove
    if (abs(gr[1]-xloc) < abs(gr[2]-xloc)) {
      grooves[[k]]$groove[1] <<- xloc
    } else {
      grooves[[k]]$groove[2] <<- xloc
    }
    output$groovePlot <- renderPlot({
      k <- as.numeric(input$k)
      p <- grooves[[k]]$plot +
        geom_vline(xintercept = grooves[[k]]$groove[1], colour="green") +
        geom_vline(xintercept = grooves[[k]]$groove[2], colour="green")

      p
    })

  })
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })

}

shinyApp(ui = ui, server = server,  options = list(height = 500))

