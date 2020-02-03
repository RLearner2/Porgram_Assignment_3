rankall <- function(outcome, num = "best") {
    rankdata <- read.csv("outcome-of-care-measures.csv")
    # Make a vector for rankings
    overallrank <- vector()
    # Get the states for looping trough
    states <- levels(rankdata[, 7])   
    # Check that the input number is within the length of the data
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
    rankdata[, 2] <- as.character(rankdata[, 2])

    # Loop creating ranking for all states
    for(i in 1:length(states)) {
        rankdatanew <- subset(rankdata, State == states[i])
        rankdatadx <- suppressWarnings(as.numeric(rankdatanew[,diagnosiscol]))
        rankdatanew <- rankdatanew[!(is.na(rankdatadx)), ]
        rankdatastate <- rankdatanew[order(rankdatanew[, diagnosiscol], rankdatanew[, 2]), ]
        # Reassign rank for input of 'best' and 'worst' to numeric values
        if(num == "best") {rankorder = 1}
        else if(num == "worst") {rankorder = nrow(rankdatastate)}
        else{rankorder = num}
        # Put the state resulting form the ranking procedure into a vector
        rankstatevect <- rankdatastate[rankorder, 2]
        # Append the vector to the output data frame
        overallrank <- append(overallrank, c(rankstatevect, states[i]))
    }
    #Format the output data frame
    overallrank <- as.data.frame(matrix(overallrank, length(states), 2, byrow = TRUE))
    #Make labels for the output data frame
    colnames(overallrank) <- c("hospital", "state")
    rownames(overallrank) <- states
    return(overallrank)
}