################################################################################
###                 Expert opinion elicitation app: probability              ###
################################################################################
library(shinyjs)
library(shiny)
library(lattice)
library(mc2d)
library(rsconnect)

#-----------------------------------
#   Function to draw plots         #
#-----------------------------------
dotplot.errors.expert <- function(x, myTheme = simpleTheme(pch = 19, col = 1), conf.level = .95, end.length = .05, reorder.Expert = TRUE, 
                                  reordering = "decreasing", reference.line = NULL, bar.color = 1, ...) 
{ require(lattice)
  o <- c(which(colnames(x) == "Expert"), which(colnames(x) == "Minimum"), which(colnames(x) == "ML"), which(colnames(x) == "Maximum"))
  if (length(o) != 4) stop("Error: Incorrect data frame")
  x <- x[, o]
  x$Expert <- factor(x$Expert)
  hor <- 1
  if (reordering == "decreasing") FUN.to.reorder <- function(x) mean(x) * hor
  else FUN.to.reorder <- function(x) -mean(x) * hor
  if (reorder.Expert) x$Expert <- with(x, reorder(Expert, ML, FUN = FUN.to.reorder))
  p <- stripplot(Expert ~ ML, data = x, Minimum = x$Minimum, Maximum = x$Maximum,
                 xlim = c(0,1),
                 xlab = "Probability",
                 ylab = "Expert",
                 pch = 16, col = "blue", cex = 2.5,
                 par.settings =list(axis.text=list(cex=1.5), 
                                    par.xlab.text=list(cex=1.4),
                                    par.ylab.text=list(cex=1.4),
                                    par.main.text = list(cex = 2)),
                 panel = function(x, y, Minimum, Maximum, ..., subscripts) {
                   if (is.null(reference.line) == F) panel.abline(v = reference.line, col = "grey")
                   panel.abline(h = y, col = "grey", lty = "dashed")
                   panel.arrows(x0 = Minimum[subscripts], y0 = y, x1 = Maximum[subscripts], y1 = y, lwd = 2, angle = 90, code = 3, length = end.length, col = bar.color)
                   panel.stripplot(x, y, ...)
                 }, ...) 
  
  print(p)
  invisible(p)  
}

dotplot.errors.expertRange <- function(x, myTheme = simpleTheme(pch = 19, col = 1), conf.level = .95, end.length = .05, reorder.Expert = TRUE, 
                                       reordering = "decreasing", reference.line = NULL, bar.color = 1, ...) 
{ require(lattice)
  o <- c(which(colnames(x) == "Expert"), which(colnames(x) == "Minimum"), which(colnames(x) == "ML"), which(colnames(x) == "Maximum"))
  if (length(o) != 4) stop("Error: Incorrect data frame")
  x <- x[, o]
  x$Expert <- factor(x$Expert)
  hor <- 1
  if (reordering == "decreasing") FUN.to.reorder <- function(x) mean(x) * hor
  else FUN.to.reorder <- function(x) -mean(x) * hor
  if (reorder.Expert) x$Expert <- with(x, reorder(Expert, ML, FUN = FUN.to.reorder))
  p <- stripplot(Expert ~ ML, data = x, Minimum = x$Minimum, Maximum = x$Maximum,
                 xlim = range(x[,2:4])+c(-.04,.04)*diff(range(x[,2:4])),
                 xlab = "Range",
                 ylab = "Expert",
                 pch = 16, col = "red", cex = 2.5,
                 par.settings =list(axis.text=list(cex=1.5), 
                                    par.xlab.text=list(cex=1.4),
                                    par.ylab.text=list(cex=1.4),
                                    par.main.text = list(cex = 2)),
                 panel = function(x, y, Minimum, Maximum, ..., subscripts) {
                   if (is.null(reference.line) == F) panel.abline(v = reference.line, col = "grey")
                   panel.abline(h = y, col = "grey", lty = "dashed")
                   panel.arrows(x0 = Minimum[subscripts], y0 = y, x1 = Maximum[subscripts], y1 = y, lwd = 2, angle = 90, code = 3, length = end.length, col = bar.color)
                   panel.stripplot(x, y, ...)
                 }, ...) 
  
  print(p)
  invisible(p)  
}

jsResetCode <- "shinyjs.reset = function() {history.go(0)}"

#-----------------------------------
#   ui section                     #
#-----------------------------------

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  useShinyjs(),                                           # Include shinyjs in the UI
  extendShinyjs(text = jsResetCode, functions = c("reset")),                      # Add the js code to the page
  actionButton("reset_button", "Reset Page"),
  
   # Application title
   titlePanel("Expert-opinion elicitation: Probability"),

     sidebarPanel(
       textInput("tableName", label ="Data name", value="Default"),
       textInput("expert", label="expert", value="1"),
       numericInput("min", label="minimum", value="0", min = 0, max = 1, step = 0.01),
       numericInput("ML", label="most likely", value="0", min = 0, max = 1, step = 0.01),
       numericInput("max", label="maximum", value="0", min = 0, max = 1, step = 0.01),
       actionButton("addButton", "add data"),
       actionButton("densButton", "density plot")
       ),
     mainPanel(
       tableOutput("table"),
       plotOutput("dotPlot"),
       plotOutput("densPlot"),
       tableOutput("summary")       
     )
)

#-----------------------------------
#   server section                 #
#-----------------------------------

server <- function(input, output) {
 
  values <- reactiveValues()
  
  values$df <- data.frame(Expert = numeric(0), Minimum = numeric(0), ML = numeric(0), Maximum = numeric(0))
  
  newEntry <- observe({
    if(input$addButton > 0) {
      isolate(values$df[nrow(values$df) + 1,] <- c(input$expert, input$min, input$ML, input$max))
      isolate(values$df$ML <-as.numeric(values$df$ML))
      isolate(values$df$Minimum <-as.numeric(values$df$Minimum))
      isolate(values$df$Maximum <-as.numeric(values$df$Maximum))
    }
  })
  
  output$table <- renderTable({values$df})
    
  output$dotPlot <- renderPlot(
    dotplot.errors.expert(values$df, reference.line = 0.5, main = input$tableName)
    )

  output$densPlot <- renderPlot(
    if(input$densButton > 0)
    {
    Data = values$df
    CombinedDistrib <- list()

    for (l in 1:length(Data$Expert)){
      iterations = 5000
      CombDist = numeric(iterations)
      for (i in 1:iterations) {
        CombDist[i] <- rpert(1, min = Data$Minimum[l], mode = Data$ML[l], max = Data$Maximum[l], shape=4)
      }
      CombinedDistrib[[l]] = CombDist
    }


    ### Plot density plots
    plot(density(CombinedDistrib[[1]]), col="green3", xlim=c(0,1), ylim = c(0, 15), main = "", xlab = "", lwd = 2)
    for (l in 2:length(Data$Expert)){
      lines(density(CombinedDistrib[[l]]), col="green3", lwd = 2)
    }

    ###  Combine all the results
    Combined <- unlist(CombinedDistrib)
    Combined <- sample(Combined, 1000, replace = TRUE)

    lines(density(Combined),col = "red", lwd = 4)
    abline(v = median(Combined), col = "blue3", lwd = 2)
    abline(v = mean(Combined), col = "orange3", lwd = 2, lty = 3)
    abline(v = (quantile(Combined, c(0.025, 0.975))), col = "green3", lwd = 2,  lty = 2)
    legend('topright', c('median', 'mean',  '95 % confidence interval'), lty=c(1, 3, 2), lwd=c(2.5, 2.5, 2.5), col=c('blue3', 'orange3', 'green3'))
    })

  output$summary <- renderTable(
    if(input$densButton > 0)
    {    Data = values$df
    CombinedDistrib <- list()

    for (l in 1:length(Data$Expert)){
      iterations = 5000
      CombDist = numeric(iterations)
      for (i in 1:iterations) {
        CombDist[i] <- rpert(1, min = Data$Minimum[l], mode = Data$ML[l], max = Data$Maximum[l], shape=4)
      }
      CombinedDistrib[[l]] = CombDist
    }

    ###  Combine all the results
    Combined <- unlist(CombinedDistrib)
    Combined <- sample(Combined, 1000, replace = TRUE)

    getmode <- function(v) {
      uniqv <- unique(v)
      uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    newLine <- table(data.frame(Mean = round(mean(Combined), 2),
                          Median = round(median(Combined), 2),
                          #Mode = round(getmode(Combined), 2),
                          CI_low = round(quantile(Combined, c(0.025)), 2),
                     CI_high = round(quantile(Combined, c(0.975)), 2)), exclude = "Freq")

  })

  observeEvent(input$reset_button, {
    js$reset()})    
  
}

# Run the application 
shinyApp(ui = ui, server = server)

