# By Anthony Layton (Tony Layton)
# 

library(psych)
library(Matrix)
library(MASS)
library(foreign)
library(irlba)
library(svd)
library(rpart)
library(ggplot2)
library(ggfortify)
library(dplyr)


customers_recommendations <- function (SVD_data, customer, available_drink_list = available_drinks) {
    cust_recommend <- SVD_data[customer,]
    avail_cust_recommend <- cust_recommend[,available_drinks]
    top_recommendations <- sort(avail_cust_recommend, decreasing = TRUE)
    top_recommendations <- format(top_recommendations, digits = 3)
    print(top_recommendations[1:5])
}


check_var <- function(SVD, number_of_factors = 800) {
    var1 <- prop.table(SVD$d^2)
    print(sum(var1[1:number_of_factors]))
}



# Import and structure the data
data <- read.csv(file = "data_RealCust_Drink_qty.csv", header = T, sep = ",", encoding = 'utf-8')
rownames(data) <- data[,1]
data <- data[,-1]
data[data == 0.0] <- 0.0
data[data < 1.0] <- 0.0  # Replaces all values less than 1 with 0
data[data == 1.0] <- 0.5  # Replaces all values equal to 1 with .5 
data[data > 1.0] <- 1.0  # Replaces all values equal to 1 with .5 greater than 0 with 1
data <- data[which(rowSums(data) != 0),] # Remove empty Columns
data <- data[,which(colSums(data) != 0)] # Remove empty Columns


# Builds list of beer that has been sold in the last 30 or 60 days.
drinks <- read.csv(file = "sold_Last60.csv", header = F, sep = ",", encoding = 'utf-8')
available_drinks <- c()
for (drink in drinks) {
    available_drinks <- drink
}


# builds SVD model
SVD <- svd(x = t(data))
check_var(SVD = SVD, number_of_factors = 326) # Check Variance # Looking for >= 70% explained
number_of_factors = 326
reduced_matrix <- (SVD$u[,1:number_of_factors] %*% diag(SVD$d[1:number_of_factors]) %*% t(SVD$v[,1:number_of_factors]))
recommendations <- reduced_matrix - data 


# Write to file
cust_names <- row.names(recommendations)
save(cust_names, file="cust_names.Rda")
save(recommendations, file="recommendations.Rda")
