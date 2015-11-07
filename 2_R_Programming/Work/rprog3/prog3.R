function <- prog3() {
    outcome <- read.csv("outcome-of-care-measures.csv", 
        colClasses = "character")

    outcome[, 11] <- as.numeric(outcome[, 11])
        #You may get a warning about NAs being introduced; 
        #that is okay
    hist(outcome[, 11])
}#prog3