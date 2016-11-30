#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(
  fluidPage(
    title = 'Visualize and download the report|Home',
    sidebarLayout(
      sidebarPanel(
        helpText('Instruction: select a variable from the drop-down list: '),
        selectInput('x', 'Build a regression model of mpg against:', choices = names(mtcars)[-1]),
        radioButtons('format', 'Download the report as: ', c('PDF', 'HTML', 'Word'), inline = TRUE),
        downloadButton('downloadReport')
      ),
      mainPanel(
        plotOutput('regPlot')
      )
    ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      sidebarPanel(
        helpText('Instruction: move the slide bar left/right to change the number of bins: '),
        sliderInput("bins",
                    "Number of bins:",
                    min = 1,
                    max = 50,
                    value = 30)
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )
  ))
  
  

# Define server logic required to draw a histogram
server <- shinyServer(
  function(input, output) {
    regFormula <- reactive({
      as.formula(paste('mpg ~', input$x))
    })
    
    # Scatterplot with the mtcars dataset
    output$regPlot <- renderPlot({
      par(mar = c(4, 4, .1, .1))
      plot(regFormula(), data = mtcars, pch = 19)
    })
    
    #Histogram with the faithful geyser dataset
    output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      x    <- faithful[, 2] 
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      # draw the histogram with the specified number of bins
      hist(x, breaks = bins, col = 'red', border = 'green')
    })
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        library(rmarkdown)
        out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )
    
  }
  
)

# Run the application 
shinyApp(ui = ui, server = server)
