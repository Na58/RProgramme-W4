rankhospital <- function(state, outcome, num = "best") {
        raw_data <- read.csv("outcome-of-care-measures.csv")
        #input validation
        if(!state %in% raw_data$State) {
                stop("invalid state")
        }
        if(!outcome %in% c("heart attack","heart failure","pneumonia")){
                stop("invalid outcome")
        }
        if(!num %in% c("best","worst") & is.numeric(num)==FALSE) {
                stop("invalid rank")
        }
        #set the column number of mortality rate according to the disease
        outcome_colno <-
                if(outcome == "heart attack") {11}
                else if(outcome == "heart failure") {17}
                else {23}
        #subset the target state
        state_data <- subset(raw_data,raw_data$State == state)
        #convert mort rate to numeric
        numeric_mort <- suppressWarnings(as.numeric(as.character(state_data[,outcome_colno])))
        #order the data
        ordered_data <- state_data[order(numeric_mort,state_data$Hospital.Name, na.last = NA),]
        rank <- 
                if(num == "best") {1}
                else if(num == "worst") {nrow(ordered_data)}
                else {as.numeric(num)}
        print(as.character(ordered_data[rank,2]))
}