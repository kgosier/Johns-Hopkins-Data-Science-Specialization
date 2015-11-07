pollutantmean <- function(directory, pollutant, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used

    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!

    x <- numeric(0)
    for (idloop in id) {
        filename <- sprintf("00%d", idloop)
        lenfn <- nchar(filename)
        filename <- substr(filename, lenfn - 2, lenfn)
        filename <- sprintf("%s/%s.csv", directory, filename)

        df <- read.csv(filename)
        dfcol <- df[[pollutant]]
        dfcol <- dfcol[!is.na(dfcol)]
        x <- append(x, dfcol)
    }
    mean(x)
}