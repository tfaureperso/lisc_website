library(shiny)
library(plotly)
library(ggplot2)
library(htmlwidgets) # to use saveWidget function

ui <- fluidPage(

  titlePanel("Plotly html widget download example"),

  sidebarLayout(
    sidebarPanel(
      # add download button
      downloadButton("download_plotly_widget", "download plotly graph")
    ),

    # Show a plotly graph
    mainPanel(
      plotlyOutput("plotlyPlot")
    )
  )

)

server <- function(input, output) {

  session_store <- reactiveValues()

  output$plotlyPlot <- renderPlotly({

    # make a ggplot graph
    g <- ggplot(faithful) + geom_point(aes(x = eruptions, y = waiting))

    # convert the graph to plotly graph and put it in session store
    session_store$plt <- ggplotly(g)

    # render plotly graph
    session_store$plt
  })

  output$download_plotly_widget <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # export plotly html widget as a temp file to download.
      saveWidget(as_widget(session_store$plt), file, selfcontained = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
