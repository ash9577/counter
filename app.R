library(shiny)
library(EBImage)
library(magrittr)
library(shinyjs)
library(png)

count_objects <- function(img) {
  gs <- channel(img, "gray")
  binary <- gs > 0.5
  inverse <- !binary
  return(max(bwlabel(inverse)))
}

ui <- fluidPage(
  
  titlePanel('Eagle eye'),
  
  sidebarLayout(
    sidebarPanel(
      fileInput(
        inputId = "file1",
        label = "Upload Image",
        accept = c('image/png', 'image/jpeg','image/jpg')
      ),
      tags$hr(),
    actionButton(inputId = "count",
                 label = "Count"),
    actionButton(inputId = "clear",
                 label = "Clear")
    ),
    mainPanel(
      textOutput(outputId = "filename"),
      imageOutput(outputId = "uploaded"),
      hr(),
      verbatimTextOutput(outputId = "ntext")
    )
  )
)

server <- function(input, output) {
  values <- reactiveValues()
  values$re1 <- reactive({
    req(input$file1)
    gsub("\\\\", "/", input$file1$datapath)
    })
  values$re2 <- reactive({
    req(input$file1)
    readImage(input$file1$datapath)
    })
  output$uploaded <- renderImage({list(src = values$re1(),
                                       width = 550)}, deleteFile = F)
  ntext <- eventReactive(input$count, { (count_objects(values$re2()))})
  output$ntext <- renderText({ntext()})
}

shinyApp(ui, server)