## Programming Assignment 3
## RankHospital.R

rankhospital <- function(state, outcome, num = "best") {
    rankdata <- read.csv("outcome-of-care-measures.csv")
    states <- rankdata[,7]
    # Check that the value for "state" is valid
    statecheck <- FALSE
    for (i in 1:length(states)) {
        if (state == states[i])  { 
            statecheck <- TRUE
        }
    }
    if (statecheck == FALSE) {
        stop("invalid state")
    }
    if (is.numeric(num) == TRUE) {
        if (length(rankdata[,2]) < num) {
            return(NA)
        }
    }
    # Check that the value for "outcome" is valid
    if(!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia")))
        stop("invalid outcome")
    # Assign outcome to the appropriate data column
    if (outcome == "heart attack") {
        diagnosiscol <- 11
    }
    else if (outcome == "heart failure") {
        diagnosiscol <- 17
    }
    else {
        diagnosiscol <- 23
    }
    # Make diagnosis column numeric and hospital name column a character string
    rankdata[, diagnosiscol] <- suppressWarnings(as.numeric(levels(rankdata[, diagnosiscol])[rankdata[, diagnosiscol]]))
    rankdata[, 2] <-as.character(rankdata[, 2])
    # Filter data to include just the input state
    rankdatanew <-subset(rankdata, State == state)
    ## Get outcome data, remove 'NAs' from data
    rankdatadx <-suppressWarnings(as.numeric(rankdatanew[, diagnosiscol]))
    rankdatanew <- rankdatanew[!(is.na(rankdatadx)), ]
    hospitallist <-rankdatanew[order(rankdatanew[, diagnosiscol], rankdatanew[, 2]), ]
    # Reassign rank for input of 'best' and 'worst' to numeric values
    if (num == "best") {
        rankorder = 1
        }
    else if (num =="worst") {
        rankorder = nrow(hospitallist)
        }
    else {
        rankorder = num
    }
    # Return the state and hospital with the requested ranking
    return(hospitallist[rankorder, 2])
}