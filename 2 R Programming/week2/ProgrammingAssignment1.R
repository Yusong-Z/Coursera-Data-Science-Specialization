# part 1
pollutantmean <- function(directory = "C:/Users/98269/Desktop/R/R Programming/week2/specdata", pollutant, id = 1:332) {
    
    setwd(directory)
    sum <- 0
    len <- 0
    
    for (i in id) {
        
        if (i < 10) { file <- paste("00", i, sep="") }
        else if (i>9 && i<100) { file <- paste("0", i, sep="") }
        else { file <- as.character(i) }
        
        data <- read.csv( paste(file, ".csv", sep="") )
        col <- data[, pollutant]
        bad <- is.na(col)
        col <- col[!bad]
        
        for (j in col) { sum <- sum + j }
        len <- len + length(col)
    }
    
    sum/len
}

# test 1
pollutantmean(pollutant="sulfate", id = 1:10)

# test 2
pollutantmean(pollutant="nitrate", id = 70:72)

# test 3
pollutantmean(pollutant="nitrate", id = 23)



# part 2
complete <- function(directory = "C:/Users/98269/Desktop/R/R Programming/week2/specdata", id = 1:332) {
    
    setwd(directory)
    nobs <- c()
    
    for (i in id) {
        
        if (i < 10) { file <- paste("00", i, sep="") }
        else if (i>9 && i<100) { file <- paste("0", i, sep="") }
        else { file <- as.character(i) }
        
        data <- read.csv( paste(file, ".csv", sep="") )
        good <- complete.cases(data)
        nobs <- append( nobs, nrow( data[good, ] ) )
    }
    
    data <- cbind(id, nobs)
    data
}

# test 1
complete(id = 1)

# test 2
complete(id = c(2, 4, 8, 10, 12))

# test 3
complete(id = 30:25)

# test 4
complete(id = 3)



# part 3
corr <- function(directory = "C:/Users/98269/Desktop/R/R Programming/week2/specdata", threshold = 0) {
    
    setwd(directory)
    ColVec <- c()
    
    for (i in 1:332) {
        
        if (i < 10) { file <- paste("00", i, sep="") }
        else if (i>9 && i<100) { file <- paste("0", i, sep="") }
        else { file <- as.character(i) }
        
        data <- read.csv( paste(file, ".csv", sep="") )
        good <- complete.cases(data)
        nocomp <- nrow( data[good, ] )
        
        if (nocomp > threshold) { 
            
            correlation <- cor( data[good, ][, "sulfate"], data[good, ][, "nitrate"] )
            ColVec <- append(ColVec, correlation) 
        }
    }
    
    ColVec
}

# test 1
cr <- corr(threshold = 150)
head(cr)
summary(cr)

# test 2
cr <- corr(threshold = 400)
head(cr)
summary(cr)

# test 3
cr <- corr(threshold = 5000)
summary(cr)
length(cr)

# test 4
cr <- corr()
summary(cr)
length(cr)