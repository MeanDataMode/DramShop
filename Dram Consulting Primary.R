# Tony Layton
# 
# Install Packages (not all of these are required)
#install.packages("MASS")
##install.packages("foreign")
#install.packages("svd")
#install.packages("irlba")
#install.packages("prcomp")
#install.packages("rpart")

library(Matrix)
library(MASS)
library(foreign)
library(irlba)
library(svd)
library(prcomp)
library(rpart)

# Read in and structure the Production Data
start.time <- Sys.time()
data = read.csv(file = "data_Cust_Drink_Percent_Row.csv", header = T, sep = ",")
rownames(data) <- data[,1]
data <- data[,-1]
data <- data[,which(colSums(data) != 0)] # Remove empty Columns
data <- data[which(rowSums(data) != 0),] # removes empty Rows
total.time <- Sys.time() - start.time
total.time
        # 26.74657 secs
        # 12.17994 secs


start.time <- Sys.time()
data = read.csv(file = "data_Cust_Cat_Percent_Row.csv", header = T, sep = ",")
rownames(data) <- data[,1]
data <- data[,-1]
data <- data[,which(colSums(data) != 0)] # Remove empty Columns
data <- data[which(rowSums(data) != 0),] # removes empty Rows
total.time <- Sys.time() - start.time
total.time




######################################
# More Stuff
start.time <- Sys.time()
O = data / sum(data)
E = rowSums(O) %o% colSums(O)
Z = (O - E) / sqrt(E)
SVD = svd(x = Z)
# SVD$u %*% diag(SVD$d) %*% t(SVD$v)
variance.explained = prop.table(svd(Z)$d^2)
total.time <- Sys.time() - start.time
total.time
        # 10.43311 mins
        # 10.03534 mins
round(variance.explained,2)
sum(round(variance.explained[1:1250],4))
#
##########


# Reducing the Matrix down to nv
start.time <- Sys.time()
svd_irlba <- irlba(t(data), nv = 700,maxit = 1400)# *(TIP:nu = nv)
reduced_matrix <- (svd_irlba$u %*% diag(svd_irlba$d) %*% t(svd_irlba$v))
total.time <- Sys.time() - start.time
total.time
        # 6.91502 secs
        # 14.7105 secs


# Shows me how far off we were 
# (-1 = THe more the negative the less we need to recomend.
# 1 = THe higher the positive the stronger the recomendation to Try.
recomendations <- reduced_matrix - data


# Tease out one persons ('John Doe' aka 'Riley Zachariasen') recomendations
john_actual <- data["Riley_Zachariasen",]
john_predicted <- recomendations["Riley_Zachariasen",]
john_recomendations <- john_predicted - john_actual


top_recomendations <- sort(john_recomendations, decreasing = TRUE)
how_many_recomendations = 5
for (n in 1:how_many_recomendations) {
        print(top_recomendations[n])
}


botched_recomendations <- sort(john_recomendations, decreasing = FALSE)
how_many_recomendations = 5
for (n in 1:how_many_recomendations) {
        print(botched_recomendations[n])
}

the_drink = "Abyss.Deschutes"
john_actual[,the_drink]
john_predicted[,the_drink]
john_recomendations[,the_drink]


# Training Data
training_svd_irlba <- svd_irlba$v


