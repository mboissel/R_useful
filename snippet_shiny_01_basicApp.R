# https://shiny.rstudio.com/tutorial/
# https://shiny.rstudio.com/gallery/kmeans-example.html

library(shiny)

####################################################################################################
#### UI : Interface ####
####

ui <- pageWithSidebar(
  headerPanel('Iris k-means clustering'),
  sidebarPanel(
    selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
    selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
    numericInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9)
  ),
  mainPanel(
    plotOutput('plot1')
  )
)



####################################################################################################
#### SERVER : get input, make object, return output ####
####

server <- function(input, output, session) {

  ## REACTIVE functions
  selectedData <- shiny::reactive({
    iris[, c(input$xcol, input$ycol)]
  })

  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })

  ## RENDER functions
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
      "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"
    ))

    par(mar = c(5.1, 4.1, 0, 1))
    plot(
      selectedData(),
      col = clusters()$cluster,
      pch = 20, cex = 3
    )
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })

}

####################################################################################################
#### RUN #### 
####
shinyApp(ui, server)


#### Autres structures ####
## si Ui et Server sont dans des scripts séparés: 
## commande runApp("~/mondossier/mon_appli")
## qqe exemples : http://www.dacta.fr/blog/r-shiny.html
