rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", 
                         colClasses = "character")
        
        ## Check that outcome is valid
        if ((outcome != "heart attack") && (outcome != "heart failure") &&
            (outcome != "pneumonia")) {
                stop("invalid outcome")
        }
        
        ## Return a data frame with the hospital names and the
        ## (abbreviated) state name
        
        if (num == "best") num <- 1
        
        ## Set column number depending on chosen condition
        if (outcome == "heart attack")  condition_col <- 11
        if (outcome == "heart failure") condition_col <- 17
        if (outcome == "pneumonia")     condition_col <- 23
        
        ## Replace data frame with only the name of the hospital, state and the
        ## chosen column
        data <- data[ , c(2, 7, condition_col)]
        
        ## Convert values in third column to numeric
        data[ , 3] <- suppressWarnings(as.numeric(data[[3]]))
        
        ## Filter out NA values
        ## data <-l data[complete.cases(data), ]
        
        if (num == "worst") {
                result <- data[order(data[[2]], -data[[3]], data[[1]]), ]
                
                states <- split(result, result$State)
                
                ranked <- lapply(states, function(x) x[1, 1])
                ranked <- unlist(ranked)
                
                answer <- data.frame(hospital = ranked, state = names(ranked), row.names = names(ranked))
        }
        else {
                result <- data[order(data[[2]], data[[3]], data[[1]]), ]
                
                states <- split(result, result$State)
                
                ranked <- lapply(states, function(x) x[num, 1])
                ranked <- unlist(ranked)
                
                answer <- data.frame(hospital = ranked, state = names(ranked), row.names = names(ranked))
        }
        
        answer
}