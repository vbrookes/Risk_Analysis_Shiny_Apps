#
#
library(shinyjs)
library(shiny)

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),                                           # Include shinyjs in the UI
  extendShinyjs(text = jsResetCode, functions = c("reset")),                      # Add the js code to the page
  actionButton("reset_button", "Reset Page"),
  
  
   # Application title
   titlePanel("Develop a distribution"),
   
   # numericInput("obs", "Observations (x):", 10, min = 1, max = 100),
   # actionButton('add','add'),
   # actionButton('density','plot density function'),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("obs", "Observations (x):", 1, min = 1, max = 100),
        actionButton('add','add'),
        actionButton('density','plot density function'),
        
        
        textOutput("Summary"),
        textOutput("median"),
        textOutput("CI"),
        textOutput("sd"),
        textOutput("Norm"),
        textOutput("TotalValue")
      ),

      # sidebarPanel(
      #   actionButton('density', "Plot density function")
      #         ),
      # 
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         plotOutput("densPlot")
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
 
  myValues <- reactiveValues()
  observe({
    if(input$add > 0){
      myValues$dList <- c(isolate(myValues$dList), isolate(input$obs))
    }
  })
  
  observe({
    if(length(myValues$dList) > 15){
      NormTest = shapiro.test(myValues$dList)
    }
  })
  
  # observe({
  #   if(input$reset > 0){
  #     myValues$dList <- NULL
  #   }
  # })
  
  output$Summary <- renderText({ 
    paste("mean = ", round(mean(myValues$dList), 2))
  })
  output$median <- renderText({ 
    paste("median = ", (median(myValues$dList)))
  })
  output$CI <- renderText({ 
    paste("95 % CI = ", round(quantile(myValues$dList, c(0.025)), 2), ' - ', round(quantile(myValues$dList, c(0.975)), 2))
  })
  output$sd <- renderText({ 
    paste("sd = ", round(sd(myValues$dList), 2))
  })
  output$TotalValue <- renderText({ 
    paste("Sum values = ", sum(myValues$dList))
  })
  
   output$distPlot <- renderPlot({
     if (!is.null(myValues$dList)) {
      # generate bins based on input$bins from ui.R
      x    <- myValues$dList
      bins <- 30
      
      # draw the histogram with the specified number of bins
      hist(x,  breaks = bins, col = 'darkred', border = 'white', main = "", 
           xlim = c(0, max(x) +3), xlab = paste("x (N = ", length(x), ")"))}
   })
   
   output$densPlot <- renderPlot({
     if(input$density > 0){
       plot(density(myValues$dList), main = "", col = 'darkred', lwd = 4, xlim = c(0, max(myValues$dList) +3), xlab = paste("x (N = ", length(myValues$dList), ")"))
       abline(v = median(myValues$dList), col = "blue3", lwd = 2)
       abline(v = mean(myValues$dList), col = "orange3", lwd = 2, lty = 3)
       abline(v = (quantile(myValues$dList, c(0.025, 0.975))), col = "green3", lwd = 2,  lty = 2)
       legend('topright', c('median', 'mean',  '95 % confidence interval'), lty=c(1, 3, 2), lwd=c(2.5, 2.5, 2.5), col=c('blue3', 'orange3', 'green3'))
     }
   })
   
   observeEvent(input$reset_button, {
     js$reset()})    
   
}

# Run the application 
shinyApp(ui = ui, server = server)
