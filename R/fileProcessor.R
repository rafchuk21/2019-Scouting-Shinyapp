
# returns an ascending sorted list of teams
# data - event data. Needs a column titled "team.." storing the team names.
funTeamList <- function(data) {
  sort(unique(data$team..))
}

# calculates various combinations of other statistics and adds them as new columns
# data - event data. Needs a lot of columns with specific names. Please don't change the app format
funComputeNewColumns <- function(data) {
  data$total.rocket.cargo <- data$level.1.cargo + data$level.2.cargo + data$level.3.cargo
  data$total.rocket.hatches <- data$level.1.hatches + data$level.2.hatches + data$level.3.hatches
  data$total.low.cargo <- data$level.1.cargo + data$ship.cargo
  data$total.low.hatches <- data$level.1.hatches + data$ship.hatches
  data$total.high.cargo <- data$level.2.cargo + data$level.3.cargo
  data$total.high.hatches <- data$level.2.hatches + data$level.3.hatches
  data$total.cargo <- data$ship.cargo + data$total.rocket.cargo
  data$total.hatches <- data$ship.hatches + data$total.rocket.hatches
  data$total.gamepieces <- data$total.hatches + data$total.cargo
  data$total.gamepiece.points <- data$total.cargo * 3 + data$total.hatches * 2
  return(data)
}

# Returns a new dataframe with each team having one row, sorted by team number, and each cell
#   contains the median of the team's values in the given column. If non-numeric, 0.
# data - event data. Needs a column named "team.."
funComputeMedians <- function(data) {
  if (nrow(data) < 1) {return(NULL)}
  tl <- funTeamList(data) # team list from data
  cn <- colnames(data) # column names of data
  
  # creates blank new dataframe, with the first and only column containing the team list from data
  dt <- data.frame("team.." = tl)
  for (i in cn) { # for each column in data
    newcol <- c() # create a new column
    for (j in tl) { # for each team
      if (is.numeric(data[1,i])) { # if the column contains numeric values
        newcol <- c(newcol, median(data[data$team.. == j, i])) # add the median of the team's data
        #                                   in the current column from input data to the new column
      } else { # if the column isn't numeric
        newcol <- c(newcol, 0) # add 0s
      }
    }
    dt <- cbind(dt, newcol) # add the new column to the new dataframe
  }
  
  dt$team.. <- NULL # delete the original column for team numbers (since it was made again)
  colnames(dt) <- colnames(data) # name the columns using the input data's column names
  return(dt) #return the new dataframe
}

# Returns a new dataframe with each team having one row, sorted by team number, and each cell
#   contains the mean of the team's values in the given column. If non-numeric, 0.
# data - event data. Needs a column named "team.."
funComputeMeans <- function(data) { # this is the exact same as funComputeMedians but with mean
  tl <- funTeamList(data)
  cn <- colnames(data)
  
  dt <- data.frame("team.." = tl)
  for (i in cn) {
    newcol <- c()
    for (j in tl) {
      if (is.numeric(data[2,i])) {
        newcol <- c(newcol, mean(data[data$team.. == j, i]))
      } else {
        newcol <- c(newcol, 0)
      }
    }
    dt <- cbind(dt, newcol)
  }
  
  dt$team.. <- NULL
  colnames(dt) <- colnames(data)
  return(dt)
}