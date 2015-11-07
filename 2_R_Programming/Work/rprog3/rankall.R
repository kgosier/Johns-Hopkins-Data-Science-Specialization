rankall <- function(outcome, num = "best") {

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
    outcome.df[[state.colname]] <- as.factor(outcome.df[[state.colname]])

    all.states <- outcome.df[[state.colname]]
    unique.states <- unique(all.states)
    unique.states <- as.character(sort(unique.states))

    num.unique.states <- length(unique.states)
    ret.df <- data.frame(hospital=character(num.unique.states),
        state=character(num.unique.states), stringsAsFactors = FALSE)
    names(ret.df) <- list("hospital","state")
    for (i in 1:length(unique.states)) {
        this.state <- unique.states[i]

        #grab state rows

        state.row.vector <- grep(toupper(this.state), 
            toupper(outcome.df[[state.colname]]))
        if (0 == length(state.row.vector)) {
            stop("invalid state")
        }
        this.outcome.df <- outcome.df[state.row.vector,]

        #rank by rate, then by name

        order.sub.vec <- order(this.outcome.df[,outcome.sub], 
            this.outcome.df[[hospital.name.colname]])
        this.outcome.df <- this.outcome.df[order.sub.vec,]

        #pull out right hospital name

        this.hospital.name <- NULL
        outlen <- length(this.outcome.df[[hospital.name.colname]])
        if (is.character(num) && tolower(num) == "best") {
            this.hospital.name <- this.outcome.df[[hospital.name.colname]][1]
        }
        else if (is.character(num) && tolower(num) == "worst") {
            this.hospital.name <- this.outcome.df[[hospital.name.colname]][outlen]
        }
        else if (!is.numeric(num)) {
            stop("invalid num")
        }
        else if (num < 1) {
            stop("invalid num")
        }
        else if (num > outlen) {
            this.hospital.name <- NA
        }
        else {
            this.hospital.name <- this.outcome.df[[hospital.name.colname]][num]
        }

        #newrow <- list(this.hospital.name, this.state)
        #ret.df <- rbind(ret.df, newrow)
        ret.df[["hospital"]][i] <- this.hospital.name
        ret.df[["state"]][i] <- this.state
    }

    ret.df
}#rankall