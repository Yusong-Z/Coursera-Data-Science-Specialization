setwd("C:/Users/98269/Desktop/R/R Programming/week4")

# part 1
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
head(outcome)

outcome[, 11] <- as.numeric(outcome[, 11]) 
hist(outcome[, 11])

# part 2
best <- function(state, OutcomeType) {
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states  <- unique(outcome[, 7])
    types   <- c("heart attack", "heart failure", "pneumonia")
    if( state %in% states == FALSE ) { stop("invalid state") }
    else if( OutcomeType %in% types == FALSE ) { stop("invalid outcome") }
    
    if (OutcomeType == types[1]) { data <- split(outcome[, c(2, 11)], outcome[, 7]) }
    else if (OutcomeType == types[2]) { data <- split(outcome[, c(2, 17)], outcome[, 7]) }
    else { data <- split(outcome[, c(2, 23)], outcome[, 7]) }
    df <- data.frame(data[state])
    df[, 2] <- as.numeric(df[, 2])
    good <- complete.cases(df)
    df <- df[good, ]
    
    BestRate <- min( df[, 2], na.rm = TRUE )
    BestHospital <- c()
    for (i in 1:nrow(df)) {
        if (  df[i, ][, 2] == BestRate ) { BestHospital <- append(BestHospital, df[i, ][, 1] ) }
    }
    BestHospital <- sort(BestHospital)
    BestHospital[1]
}

# test 1
best("TX", "heart attack")

# test 2
best("TX", "heart failure")

# test 3
best("MD", "heart attack")

# test 4
best("MD", "pneumonia")

# test 5
best("BB", "heart attack") 

# test 6
best("NY", "hert attack") 



# part 2
rankhospital <- function(state, OutcomeType, num = "best") {
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    states  <- unique(outcome[, 7])
    types   <- c("heart attack", "heart failure", "pneumonia")
    if( state %in% states == FALSE ) { stop("invalid state") }
    else if( OutcomeType %in% types == FALSE ) { stop("invalid outcome") }
    
    if (OutcomeType == types[1]) { data <- split(outcome[, c(2, 11)], outcome[, 7]) }
    else if (OutcomeType == types[2]) { data <- split(outcome[, c(2, 17)], outcome[, 7]) }
    else { data <- split(outcome[, c(2, 23)], outcome[, 7]) }
    df <- data.frame(data[state])
    df[, 2] <- as.numeric(df[, 2])
    good <- complete.cases(df)
    df <- df[good, ]
    
    df <- df[order(df[, 2], df[, 1]), ]
    df$rank <- 1:nrow(df)
    
    result <- "NA"
    if (num == "best") {num <- 1}
    else if (num == "worst") (num <- max(df$rank) )
    
    for (i in df$rank) {
        if ( df[i, ][, 3] == num ) {
            result <- df[i, ][, 1]
        }
    }
    
    result
}

# test 1
rankhospital("TX", "heart failure", 4)

# test 2
rankhospital("MD", "heart attack", "worst")

# test 3
rankhospital("MN", "heart attack", 5000)



# part 4
rankall <- function(OutcomeType, num = "best") {
    
    outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    types   <- c("heart attack", "heart failure", "pneumonia")
    if( OutcomeType %in% types == FALSE ) { stop("invalid outcome") }
    
    states    <- unique(outcome[, 7])
    hospitals <- c()
    for (i in states) {
        hospitals <- append(hospitals, rankhospital(i, OutcomeType, num))
    }
    
    table <- cbind(hospitals, states)
    table <- table[order(table[, 2]), ]
    table
}

# test 1
head(rankall("heart attack", 20), 10)

# test 2
tail(rankall("pneumonia", "worst"), 3)

# test 3
tail(rankall("heart failure"), 10)



# Quiz
# 1
best("SC", "heart attack")

# 2
best("NY", "pneumonia")

# 3
best("AK", "pneumonia")

# 4
rankhospital("NC", "heart attack", "worst")

# 5
rankhospital("WA", "heart attack", 7)

# 6
rankhospital("TX", "pneumonia", 10)

# 7
rankhospital("NY", "heart attack", 7)

# 8
r <- rankall("heart attack", 4)

# 9
r <- rankall("pneumonia", "worst")

# 10
r <- rankall("heart failure", 10)