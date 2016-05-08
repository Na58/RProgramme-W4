best <- function(state, outcome) {
        #read outcome data
        rawmeasure <- read.csv("outcome-of-care-measures.csv")
        #input validation
        # print(rawmeasure$State)
        if(!state %in% rawmeasure$State) {
                stop ("invalid state")
        }
        if (!outcome %in% c("heart attack","heart failure","pneumonia")){
                stop("invalid outcome")
        }
        #find the sequence
        if(outcome == "heart attack") {
                col_no <- 11
        }
        if(outcome == "heart failure") {
                col_no <- 17
        }
        if(outcome == "pneumonia") {
                col_no <- 23
        }
        #new data frame containing only the input state
        new_measure <- subset(rawmeasure, rawmeasure$State==state)
        #get lowest mortality
        numeric_mort <- suppressWarnings(as.numeric(as.character(new_measure[,col_no])))
        lowest_mort <- min(numeric_mort,na.rm=TRUE)
        #get the data frame with the lowest mortality
        lowest_record <- subset(new_measure, numeric_mort==lowest_mort)
        #first in alphabetical order in case of ties
        ordered_record <- lowest_record[order(lowest_record$Hospital.Name),]
        print(as.character(ordered_record[1,]$Hospital.Name))
}