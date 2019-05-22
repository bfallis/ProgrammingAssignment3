rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                colClasses = "character")
        
        ## Check that state is valid
        if (!(state %in% data[, 7])) stop("invalid state")
        
        ## Check that outcome is valid
        if ((outcome != "heart attack") && (outcome != "heart failure") &&
            (outcome != "pneumonia")) {
                stop("invalid outcome")
        }
        
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        
        if (num == "best") num <- 1
        
        ## Select only rows for the chosen state
        data <- data[data$State == state, ]
        
        ## Set column number depending on chosen condition
        if (outcome == "heart attack")  condition_col <- 11
        if (outcome == "heart failure") condition_col <- 17
        if (outcome == "pneumonia")     condition_col <- 23
        
        ## Replace data frame with only the name of the hospital and the
        ## chosen column
        data <- data[ , c(2, condition_col)]
        
        ## Convert values in second column to numeric
        data[ , 2] <- suppressWarnings(as.numeric(data[[2]]))
        
        ## Filter out NA values
        data <- data[complete.cases(data), ]
        
        if (num == "worst") {
                result <- data[order(-data[[2]], data[[1]]), ]
                return(result[1, 1])
        }
        else {
                result <- data[order(data[[2]], data[[1]]), ]
                return(result[num, 1])
        }
        
}