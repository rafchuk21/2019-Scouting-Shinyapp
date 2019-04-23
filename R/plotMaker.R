library(ggplot2)
source("fileProcessor.R")

# Creates a violin plot of at least one team using a given variable
# data - data to make a plot of (requires column named "team..")
# teamsToPlot - list of teams to make violins for
# variable - name of variable column in data to make violin plots of
# title - title of plot
# xlabel - label for x axis
# ylabel - label for y axis
# order - permutation vector for ordering teams. If NULL, orders by variable. Use order() function.
#         Has to have same length as teamsToPlot
# decreasingOrder - whether data should be ordered by variable in decreasing order
multiTeamViolinPlot <- function(data, teamsToPlot, variable,
                                title = paste("Violin Plot of", variable),
                                xlabel = "Team Number", ylabel = variable,
                                order = NULL, decreasingOrder = TRUE) {
  r = range(data[,variable]) #range of selected variable
  filteredData <- data[data$team.. %in% teamsToPlot, c("team..", variable)] #filter by team
  if (is.null(teamsToPlot)) { return(NULL) } #return null when no teams are selected
  #if no order is defined, order the teams by medians
  if (is.null(order)) {
    medData <- funComputeMedians(filteredData) #compute median data
    order <- order(medData[,variable], decreasing = decreasingOrder) #create the order vector
  }
  
  #store the team names with their assigned order
  xValues <- factor(filteredData$team.., levels = teamsToPlot[order])
  #make sure to get rid of NA values
  xValues <- xValues[!is.na(xValues)]
  #base ggplot
  p <- ggplot(filteredData,
              aes(xValues, #x values
                  filteredData[,variable])) #y values
  #add violin plots
  p <- p + geom_violin(aes(fill = factor(team..)))
  #add y axis
  p <- p + scale_y_continuous(limits = r, #set limits of axis
                              breaks = seq(r[1], r[2], by = ceiling((r[2]-r[1])/15)), #set major break spacing
                              minor_breaks = NULL) #set minor break spacing
  #add axis labels
  p <- p + labs(title = title, #main title
                y = ylabel, #y axis label
                x = xlabel) #x axis label
  #add boxplots over violin plots
  p <- p + geom_boxplot(width = .1)
  #get rid of legend
  p <- p + theme(legend.position = "none")
  return(p)
}