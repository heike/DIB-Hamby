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


resolution <- bullets$x3p[[1]] %>% x3p_get_scale()
dims <- dim(bullets$x3p[[1]]$surface.matrix)

# for quick lookup:
# idx is assumed to be the list of indices in bullets that should
# be investigated
lookup <- idx
names(lookup) <- bullets$land_id[idx]

ui <- fluidPage(

  # Application title
  titlePanel("Check crosscut"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("scans", "Select scan", choices = bullets$land_id[idx], selected = 1),
      sliderInput("crosscut",
                  "Crosscut value:",
                  min = 0,
                  max = dims[2]*resolution,
                  value = bullets$cc[idx[1]],
                  step=resolution),
      actionButton("ok", "OK")
    ),

    # Show a plot of the generated distribution
    mainPanel(
      rglwidgetOutput("rawScan",width = "100%", height="200px") %>%
        withSpinner(color="#228B22")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  idxi <- reactive({
 #   if (is.null(input$scans)) return(NULL)
    lookup[input$scans]
  })

  x3p <- reactive({
#    browser()

    x3p <- bullets$x3p[[idxi()]] %>% sample_x3p(m=3)

    x3p <- x3p %>% x3p_add_hline(yintercept = bullets$cc_pred[idxi()], size=3, color="white")
    x3p %>% x3p_add_hline(yintercept = input$crosscut, size=3, color="yellow")
  }
  )

  observeEvent(input$ok, {


    bullets$cc[idxi()] <<- input$crosscut
#    browser()
    nextidxi <- which(lookup==idxi())
    if (nextidxi < length(lookup))
    updateSelectInput(session, "scans", selected=names(lookup)[nextidxi+1])
  })

  output$rawScan <- renderRglwidget({
    try(close3d())
    x3p <- x3p()

    if (is.null(x3p)) return()
    dims <- dim(x3p$surface.matrix)
    p <- x3p %>% x3ptools::x3p_image(
      size = c(200*dims[1]/dims[2], 200), zoom = .5)

    #    p <- markers::x3p_image(x3p, width=1000)
    p2 <- rglwidget(shinyBrush=input$mouse,
                    reuse = TRUE) # , elementId = "rglScan") # shiny doesn't use the elementId

    p2
    #    p2 %>% rgl::rglMouse(choices = c("trackball", "selecting"))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
