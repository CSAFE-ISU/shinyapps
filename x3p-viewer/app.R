#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rgl)

library(bulletr)

options(rgl.useNULL = TRUE)
options(shiny.maxRequestSize=100*1024^2) 

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("x3p-viewer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("example", "Examples", c("LEA", "Breech Face", "Firing Pin"), selected = "LEA"),
      fileInput("file", "Select File (x3p or x-y-z format)"),
      sliderInput("sample",
                  "Downsampling factor:",
                  min = 1,
                  max = 15,
                  value = 4),
      selectInput("light", "Light source on:", choices=c("Left", "Top", "Right", "Bottom"), selected = "Left"),
      actionButton("show","Display")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      rglwidgetOutput('thewidget1'), #, width = "800px", height="800px"),
      tableOutput("info")
    )
  )
)
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  xmin <- xmax <- ymin <- ymax <- zmax <- NULL
  values <- reactiveValues(x3p=br411)
  
  read_input <- function(input) {
    file = input$file$datapath
    if (is.null(file)) {
      if (input$example == "LEA") scan <- br411
      if (input$example == "Breech Face") load("data/scan.rda")
      if (input$example == "Firing Pin") {
        load("data/cc.rda")
        scan <- cc
      }
    } else {
      if (length(grep("x3p$",file)) > 0) scan <- read_x3p(path = file)
      if (length(grep("dat$",file)) > 0) {
        scan <- read_dat(path = file)
      }
    }  
    scan
  }
  
  showScene <- function(x, y, z) {
    params <- rgl::r3dDefaults
    params$userMatrix <- diag(c(1,1,1,1))
    open3d(params=params)
    aspect3d(1,1,1)
    rgl.pop("lights")
    surface3d(x, y, z, color = "#cd7f32", back = "lines")
    if (input$light == "Left") {
      xyz <- matrix(c(min(x)-3*diff(range(x)), mean(y), max(z, na.rm=TRUE)), ncol = 3)
    }
    if (input$light == "Right") {
      xyz <- matrix(c(max(x)+3*diff(range(x)), mean(y), max(z, na.rm=TRUE)), ncol = 3)
    }
    if (input$light == "Top") {
      xyz <- matrix(c(mean(x), max(y)+3*diff(range(y)), max(z, na.rm=TRUE)), ncol = 3)
    }
    if (input$light == "Bottom") {
      xyz <- matrix(c(mean(x), min(y)-3*diff(range(y)), max(z, na.rm=TRUE)), ncol = 3)
    }
    
    light3d(x = xyz, diffuse = "gray40",
            specular = "gray40", ambient="grey10", viewpoint.rel = TRUE)
    light3d(diffuse = "gray20", specular = "gray20")
    scene1 <- scene3d()
    
    rgl.close()
    scene1
  }
  
  
  scene1 <- eventReactive(input$show, {
    values$x3p <- read_input(input)
    
    surfmat <- values$x3p$surface.matrix
    sample = input$sample
    if (sample != 1) {
      #      browser()
      xindx <- seq.int(from = 1, to = dim(surfmat)[1], by = sample)
      yindx <- seq.int(from = 1, to = dim(surfmat)[2], by = sample)
      surfmat <- surfmat[xindx, yindx]
    }
    
    if (prod(dim(surfmat)) > 150000) {
      session$sendCustomMessage(type = 'testmessage',
                                message = 'The image is too large to be shown (in a reasonable amount of time). Increase downsampling factor.')
      
      return(NULL)
    }
    z <- 2*surfmat/sample # Exaggerate the relief
    y <- values$x3p$header.info$profile_inc * (1:ncol(z)) # 
    x <- values$x3p$header.info$obs_inc * (1:nrow(z)) # 
    xmin <<- min(x)
    xmax <<- max(x)
    ymin <<- min(y)
    ymax <<- max(y)
    zmax <<- max(z, na.rm=TRUE)
#    zlim <- range(z, na.rm=TRUE)
#    zlen <- zlim[2] - zlim[1] + 1
    
    showScene(x, y, z)
  })
  
  output$thewidget1 <- renderRglwidget({
    #   browser()
    rglwidget(scene1())
  })
  
  output$info <- renderTable({
    vals <- unlist(values$x3p$general.info)
    if (is.null(vals)) res <- data.frame("No meta data available"=integer(0))
    else res <- data.frame(Names=names(vals), Values=vals)
    res
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

