# import data
hw1 <- read.csv("hw1_data.csv")


# question 17
col <- hw1[, "Ozone"]
bad <- is.na(col)
mean(col[!bad])

# question 18
data <- hw1[hw1$Ozone > 31, ]
data <- data[data$Temp > 90, ]
good <- complete.cases(data)
mean(data[good, ][, "Solar.R"])

# queation 19
data <- hw1[hw1$Month == 6, ]
mean(data[, "Temp"])

# question 20
data <- hw1[hw1$Month == 5, ]
oz <- data[, "Ozone"]
bad <- is.na(oz)
max(oz[!bad])