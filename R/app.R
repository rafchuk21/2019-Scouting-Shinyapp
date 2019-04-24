#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

ui <- fluidPage(
   
   titlePanel("449 Scouting Data Display"),
   
   sidebarLayout(
     sidebarPanel(
       fileInput("file", "Scouting csv File"),
       fileInput("matchSchedule", "Match Schedule csv File"),
       conditionalPanel(
         condition = "output.displayMatchSched",
         textInput("matchSelection", "Match #")
       ),
       checkboxGroupInput("displayType", "Display", choices = list(
         "Table" = 1, "Histogram" = 2, "Violin Plot" = 3)),
       conditionalPanel(
         condition = "output.displaySingleTeamSelector",
         uiOutput("singleTeamSelector")
       ),
       conditionalPanel(
         condition = "output.displayHistogram",
         sliderInput("numBins", "Number of Bins", min = 4, max = 16, value = 8)
       ),
       conditionalPanel(
         condition = "output.displayMultiTeamSelector",
         uiOutput("multiTeamSelector")
       ),
       conditionalPanel(
         condition = "output.variableRadioButtons",
         radioButtons("plotVariables", "Variable: ", choices = list(
           "Total Gamepiece Points" = "total.gamepiece.points",
           "Total Gamepieces" = "total.gamepieces",
           "Total Cargo" = "total.cargo",
           "Total Hatches" = "total.hatches",
           "Rocket Cargo" = "total.rocket.cargo",
           "Rocket Hatches" = "total.rocket.hatches",
           "Low Cargo" = "total.low.cargo",
           "Low Hatches" = "total.low.hatches"))
       )
      ),
     
      mainPanel(
        conditionalPanel( #data table display panel
          condition = "output.displayTable",
          tableOutput("bigTable")
        ),
        conditionalPanel( #histogram display panel
          condition = "output.displayHistogram",
          plotOutput("histogram")
        ),
        conditionalPanel (#violin plot display panel
          condition = "output.displayViolinPlot",
          plotOutput("violinPlot"))
      )
   )
)

server <- function(input, output) {
  
  source("fileProcessor.R")
  source("plotMaker.R")
  
  finalData <- reactive ({
    if (is.null(input$file$datapath)){ return(NULL) }
    data <- read.csv(input$file$datapath)
    if (is.null(data)) { return(NULL) }
    return(funComputeNewColumns(data))
  })
  
  matchSchedule <- reactive({
    if (is.null(input$matchSchedule$datapath)) { return(NULL)}
    matchSched <- read.csv(input$matchSchedule$datapath)
    if (is.null(data)) { return(NULL) }
    return (matchSched)
  })
  
  teamList <- reactive({
    if (fileLoaded())
      return(funTeamList(finalData()))
    return ("Select File")
  })
  
  computeMedians <- reactive({
    if (fileLoaded())
      return(funComputeMedians(finalData()))
  })
  
  computeMeans <- reactive({
    if (fileLoaded())
      return(funComputeMeans(finalData()))
  })
  
  maxNumMatches <- reactive({
    if (!fileLoaded()) return (NULL)
    max <- 0
    for (i in teamList()) {
      if (length(finalData()[finalData()$team.. == i,1]) >= max) {
        max <- length(finalData()[finalData()$team.. == i,1])
      }
    }
    return(max)
  })
  
  fileLoaded <- reactive({
    !is.null(finalData())
  })
  
  displayMatchSched <- reactive ({
    !is.null(matchSchedule())
  })
  output$displayMatchSched <- reactive ({
    displayMatchSched()
  })
  
  displayTable <- reactive ({
    fileLoaded() & (1 %in% input$displayType)
  })
  output$displayTable <- reactive({
    displayTable()
  })
  
  displayHistogram <- reactive({
    fileLoaded() & (2 %in% input$displayType)
  })
  output$displayHistogram <- reactive({
    displayHistogram()
  })
  
  
  displayViolinPlot <- reactive({
    fileLoaded() & (3 %in% input$displayType)
  })
  output$displayViolinPlot <- reactive({
    displayViolinPlot()
  })
  
  output$displaySingleTeamSelector <- reactive({
    return (displayTable() | displayHistogram())
  })
  
  output$displayMultiTeamSelector <- reactive({
    return (displayViolinPlot())
  })
  
  output$variableRadioButtons <- reactive({
    return(displayHistogram() | displayViolinPlot())
  })
  
  outputOptions(output, "displayMatchSched", suspendWhenHidden = FALSE)
  outputOptions(output, "displayTable", suspendWhenHidden = FALSE)
  outputOptions(output, "displayHistogram", suspendWhenHidden = FALSE)
  outputOptions(output, "displayViolinPlot", suspendWhenHidden = FALSE)
  outputOptions(output, "displaySingleTeamSelector", suspendWhenHidden = FALSE)
  outputOptions(output, "displayMultiTeamSelector", suspendWhenHidden = FALSE)
  outputOptions(output, "variableRadioButtons", suspendWhenHidden = FALSE)
  
  output$teams <- reactive({
    toString(teamList())
  })
  
  #UI element for single team selection
  output$singleTeamSelector <- renderUI({
    h <- ""
    if (displayTable()) {
      h <- paste(h, "Table, ", sep = "")
    }
    if (displayHistogram()) {
      h <- paste(h, "Histogram, ", sep = "")
    }
    
    h <- paste(substr(h, 1, nchar(h)-2), " Team Selector: ", sep = "")
    selectInput("singleTeamSelection", h, choices = c("All", teamList()))
  })
  
  #UI element for multiple team selection
  output$multiTeamSelector <- renderUI({
    h <- ""
    if (displayViolinPlot()) {
      h <- paste(h, "Violin Plot, ", sep = "")
    }
    
    h <- paste(substr(h, 1, nchar(h)-2), " Team Selector: ", sep = "")
    fluidRow(
      checkboxGroupInput("dummy1", h, choices=c()),
      column(
        width = 4,
        checkboxGroupInput("multiTeamSelection1", "", choices = teamList()[1:floor(length(teamList())/2)])
      ),
      column(
        width = 4,
        checkboxGroupInput("multiTeamSelection2", "", choices = teamList()[(floor(length(teamList())/2)+1):length(teamList())])
      )
    )
  })
  
  outputOptions(output, "singleTeamSelector", suspendWhenHidden = FALSE)
  outputOptions(output, "multiTeamSelector", suspendWhenHidden = FALSE)
  
  matchSelection <- reactive({
    selection <- as.numeric(input$matchSelection)
    if (is.null(selection)) { return(NULL) }
    print(selection)
    if (is.na(selection) | selection < 1 | selection > nrow(matchSchedule())) { return(NULL) }
    teams <- as.numeric(matchSchedule()[input$matchSelection,])
    print(teams)
    return(teams)
  })
  
  multiTeamSelectorResult <- reactive({
    unique(c(input$multiTeamSelection1, input$multiTeamSelection2, matchSelection()), fromLast = TRUE)
  })
  
  output$histograms <- renderUI({
    
  })
  
  output$teamSelected <- renderText({
    input$singleTeamSelection
  })
  
  filteredTable <- reactive({
    if (!fileLoaded()) return (NULL)
    if (input$singleTeamSelection == "All")
      return(finalData())
    return(finalData()[finalData()$team.. == input$singleTeamSelection,])
  })
  
  output$bigTable <- renderTable({
    if (!fileLoaded()) return (NULL)
    columns <- c("team..", "match..", "total.cargo", "total.hatches")
    return(filteredTable()[,columns])
  })
  
  #creates the histograms
  output$histogram <- renderPlot({
    if (!fileLoaded()) return (NULL) #don't crash if no file is loaded
    variable <- input$plotVariables #variable to plot on histogram
    #breaks on y axis
    b = (seq(range(finalData()[,variable])[1], #min is min of range
                       range(finalData()[,variable])[2], #max is max on range
                       length.out=input$numBins)) #make input number of divisions
    if (!input$singleTeamSelection == "All")
      return(hist(filteredTable()[,variable], main = paste("Histogram of ", variable),xlim = range(b), breaks = b, freq = TRUE, ylim = c(0, maxNumMatches())))
    return(hist(filteredTable()[,variable], main = paste("Histogram of ", variable), xlim = range(b), breaks = b, freq = TRUE))
  })
  
  #creates the violin plots
  output$violinPlot <- renderPlot({
    if (!fileLoaded()) return(NULL) #don't crash if no file is loaded
    variable <- input$plotVariables #variable to plot on violin plot
    violinPlotTeams <- multiTeamSelectorResult() #teams to plot on violin plot
    data <- finalData()
    order <- NULL
    if (!is.null(matchSelection())) {
      order <- 1:violinPlotTeams
    }
    return(multiTeamViolinPlot(data, violinPlotTeams, variable, order = order))
  })
}

shinyApp(ui = ui, server = server)

