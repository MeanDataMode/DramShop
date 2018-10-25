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


# Import and structure the data
start.time <- Sys.time()
data = read.csv(file = "data_Cust_Drink_Percent_Row.csv", header = T, sep = ",")
rownames(data) <- data[,1]
data <- data[,-1]
data <- data[,which(colSums(data) != 0)] # Remove empty Columns
data <- data[which(rowSums(data) != 0),] # removes empty Rows
total.time <- Sys.time() - start.time
total.time


######################################
# Build SVD model and check variance
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
number_of_factors = 735
sum(variance.explained[1:number_of_factors])  # Looking for >= 70% explained


# Reducing the Matrix down to nv
# the irlba is a faster way of doing SVD
start.time <- Sys.time()
svd_irlba <- irlba(t(data), nv = number_of_factors,maxit = (number_of_factors*2))  # (TIP:nu = nv)
reduced_matrix <- (svd_irlba$u %*% diag(svd_irlba$d) %*% t(svd_irlba$v))
total.time <- Sys.time() - start.time
total.time


# Shows me how far off we were 
# (-1 = THe more the negative the less we need to recomend. (our model underestimated the customers purchases)
# 1 = THe higher the positive the stronger the recomendation to Try.
recomendations <- reduced_matrix - data


# Look at an actual customers recomendations ('John Doe' aka 'Riley Zachariasen')
johns_actual <- data["Riley_Zachariasen",]
johns_predicted <- recomendations["Riley_Zachariasen",]
johns_recomendations <- johns_predicted - johns_actual

# Top Five to recommend to customer
top_recomendations <- sort(johns_recomendations, decreasing = TRUE)
how_many_recomendations = 5
for (n in 1:how_many_recomendations) {
        print(top_recomendations[n])
}


# Top 5 drinks that the customer purchased more than our model anticipated
botched_recomendations <- sort(johns_recomendations, decreasing = FALSE)
how_many_recomendations = 5
for (n in 1:how_many_recomendations) {
        print(botched_recomendations[n])
}


one = "DesmetBelgianRed.Blacksmith"
two = "KatabaticButte.CallIrishRed" 
three = "SteepN.Deep.LonePeak" 
four = "APIPA.Katabatic" 
five = "MiamiVicePineappleIPA.DraughtWorks"
the_drink = one
john_actual[,the_drink]
john_predicted[,the_drink]
john_recomendations[,the_drink]

