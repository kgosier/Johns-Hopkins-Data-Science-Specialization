complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files

    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
        
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases

    save_id <- integer(0)
    save_nobs <- integer(0)
    for (idloop in id) {
        filename <- sprintf("00%d", idloop)
        lenfn <- nchar(filename)
        filename <- substr(filename, lenfn - 2, lenfn)
        filename <- sprintf("%s/%s.csv", directory, filename)

        df <- read.csv(filename)
        df <- df[complete.cases(df),]

        save_id <- append(save_id, idloop)
        save_nobs <- append(save_nobs, nrow(df))
    }
    data.frame(id=save_id,nobs=save_nobs)
}