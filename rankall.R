rankall <- function(outcome,num = "best") {
        raw_data <- read.csv("outcome-of-care-measures.csv")
        #input validation
        if (!outcome %in% c("heart attack","heart failure","pneumonia")){
                stop("invalid outcome")
        }
        if(!num %in% c("best","worst") & is.numeric(num)==FALSE) {
                stop("invalid rank")
        }
        #identify the target disease column
        outcome_colno <-
                if(outcome == "heart attack") {11}
                else if(outcome == "heart failure") {17}
                else {23}
        #convert the mort rate to numeric
        numeric_record <- suppressWarnings(as.numeric(as.character(raw_data[,outcome_colno])))
        #order the data by mort rate, state, and hospital name
        ordered_record <- raw_data[order(numeric_record, raw_data$State, 
                                         raw_data$Hospital.Name,na.last = NA),]
        #get all the states
        states <- as.character(levels(raw_data$State))
        #define an empty data frame
        nation_ranks <- data.frame()
        #loop though the states to get the rank
        for(i in 1:length(states)) {
                state_record <- subset(ordered_record,ordered_record$State == states[i])
                rank <- 
                        if(num == "best") {1}
                        else if(num == "worst") {nrow(state_record)}
                        else{as.numeric(num)}
                target_rank <- state_record[rank,]
                nation_ranks <- rbind(nation_ranks,target_rank)
        }
        #get hospital and state, bind it as a dataframe
        hospital <- as.character(nation_ranks[,2])
        state <- as.character(nation_ranks[,7])
        rank_all_nation <- data.frame(cbind(hospital,state))
        return(rank_all_nation)
}