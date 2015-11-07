corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0

    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!

    complete_df <- complete(directory)
    retcorr <- numeric(0)
    for (i in 1:nrow(complete_df)) {
        if (complete_df[["nobs"]][i] >= threshold) {
            filename <- sprintf("00%d", complete_df[["id"]][i])
            lenfn <- nchar(filename)
            filename <- substr(filename, lenfn - 2, lenfn)
            filename <- sprintf("%s/%s.csv", directory, filename)

            df <- read.csv(filename)
            df <- df[complete.cases(df),]

            s <- df[["sulfate"]]
            n <- df[["nitrate"]]
            retcorr <- append(retcorr, cor(s,n))
        }
    }
    retcorr
}