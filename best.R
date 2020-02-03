best <- function(state, outcome) {
    caredata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states <- caredata[,7]
    statecheck <- FALSE
    for (i in 1:length(states)) {
        if (state == states[i])  {
            statecheck <- TRUE
        }
    }
    if (statecheck == FALSE) {
        stop("invalid state")
    }
    if(!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia")))
        stop("invalid outcome")
    if(outcome == "heart attack") {
        diagnosiscol <- 11
    }
    else if(outcome == "heart failure") {
        diagnosiscol <- 17
    }
    else {
        diagnosiscol <- 23
    }
    ## Get the state data
    caredatanew <-subset(caredata, State == state)
    ## Get the outcome data, clean outcome and state data
    caredatadx <-suppressWarnings(as.numeric(caredatanew[, diagnosiscol]))
    caredatanew <- caredatanew[!(is.na(caredatadx)), ]
    caredatadx <-as.numeric(caredatanew[, diagnosiscol])
    #Sort for highest ranking (smaller is better)
    hospitallist <-which(caredatadx == min(caredatadx))
    besthospitals <- caredatanew[hospitallist, 2]
    # Sort alphabetically in case of ranking ties
    besthospital <-sort(besthospitals)
    return(besthospital)
}