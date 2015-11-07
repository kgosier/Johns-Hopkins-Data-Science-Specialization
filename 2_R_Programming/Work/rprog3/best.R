best <- function(state, outcome) {

    #constants/params etc

    outcome.file <- "outcome-of-care-measures.csv"
    srch.outcome.stub <- "hospital.30.day.death..mortality..rates.from."
    state.colname <- "State"
    hospital.name.colname <- "Hospital.Name"
    missing.char <- "Not Available"

    # Read outcome data

    outcome.df <- read.csv(outcome.file, colClasses = "character")

    #search for this outcome

    srch.outcome <- sprintf("^%s%s$", srch.outcome.stub, outcome)
    srch.outcome <- gsub(" ", ".", srch.outcome)
    outcome.sub <- grep(tolower(srch.outcome), tolower(names(outcome.df)))
    if (length(outcome.sub) != 1) {
        stop("invalid outcome")
    }
    missing.rows <- grepl(missing.char, outcome.df[,outcome.sub])
    outcome.df <- outcome.df[!missing.rows,]
    outcome.df[,outcome.sub] <- as.numeric(outcome.df[,outcome.sub])

    #grab state rows

    state.row.vector <- grep(toupper(state), 
        toupper(outcome.df[[state.colname]]))
    if (0 == length(state.row.vector)) {
        stop("invalid state")
    }
    outcome.df <- outcome.df[state.row.vector,]

    #rank by rate, then by name

    order.sub.vec <- order(outcome.df[,outcome.sub], 
        outcome.df[[hospital.name.colname]])
    outcome.df <- outcome.df[order.sub.vec,]

    outcome.df[[hospital.name.colname]][1]
}#best