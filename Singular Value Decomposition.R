# https://stats.idre.ucla.edu/r/codefragments/svd_demos/


# Example 1: SVD to find a generalized inverse of a non-full-rank matrix
install.packages("MASS")
library(MASS)


MyMatrix <- matrix(c('P1','P2','P3','P4','P5','P6','P7',
                     1,3,4,5,0,0,0,
                     1,3,4,5,2,0,1,
                     1,3,4,5,0,0,0,
                     0,0,0,0,4,5,2,
                     0,0,0,0,4,5,2), 7, 6)

a = MyMatrix

a.svd <- svd(a)
a.svd$d


ds <- diag(1/a.svd$d[1:3])
u <- a.svd$u
v <- a.svd$v
us <- as.matrix(u[, 1:3])
vs <- as.matrix(v[, 1:3])

(a.ginv <- vs %*% ds %*% t(us))

# using the function ginv defined in MASS
ginv(a)




# Example 2: Image processing
install.packages("ReadImages")
library(ReadImages)
x <- read.jpeg("pansy.jpg")
dim(x)

plot(x, useRaster = TRUE)

r <- imagematrix(x, type = "grey")

plot(r, useRaster = TRUE)

r.svd <- svd(r)
d <- diag(r.svd$d)
dim(d)

u <- r.svd$u
v <- r.svd$v
plot(1:length(r.svd$d), r.svd$d)

# first approximation
u1 <- as.matrix(u[-1, 1])
v1 <- as.matrix(v[-1, 1])
d1 <- as.matrix(d[1, 1])
l1 <- u1 %*% d1 %*% t(v1)
l1g <- imagematrix(l1, type = "grey")
plot(l1g, useRaster = TRUE)

# more approximation
depth <- 5
us <- as.matrix(u[, 1:depth])
vs <- as.matrix(v[, 1:depth])
ds <- as.matrix(d[1:depth, 1:depth])
ls <- us %*% ds %*% t(vs)
lsg <- imagematrix(ls, type = "grey")

plot(lsg, useRaster = TRUE)




# Example 3: Principal components analysis using SVD
install.packages("foreign")
library(foreign)

auto <- read.dta("http://statistics.ats.ucla.edu/stat/data/auto.dta")

pca.m1 <- prcomp(~trunk + weight + length + headroom, data = auto,
                 scale = TRUE)

screeplot(pca.m1)

# spectral decomposition: eigen values and eigen vectors
xvars <- with(auto, cbind(trunk, weight, length, headroom))
corr <- cor(xvars)
a <- eigen(corr)
(std <- sqrt(a$values))


(rotation <- a$vectors)


df <- nrow(xvars) - 1
zvars <- scale(xvars)
z.svd <- svd(zvars)
z.svd$d/sqrt(df)


z.svd$v






# Example 4: Metric multi-dimensional scaling with SVD
cnut <- read.dta("http://statistics.ats.ucla.edu/stat/data/cerealnut.dta")
cnut <- matrix(c('P1','P2','P3','P4','P5','P6','P7',
                     1,3,4,5,0,0,0,
                     1,3,4,5,2,0,1,
                     1,3,4,5,0,0,0,
                     0,0,0,0,4,5,2,
                     0,0,0,0,4,5,2), 7, 6)
cnut

colnames(cnut) <- c("People",
                    "Matrix",
                    "Alien",
                    "Serenity",
                    "Casablanca",
                    "Amelie")

cnut

# centering the variables
mds.data <- as.matrix(sweep(cnut[, -1], 2, colMeans(cnut[, -1])))
dismat <- dist(mds.data)
mds.m1 <- cmdscale(dismat, k = 8, eig = TRUE)
mds.m1$eig

mds.m1 <- cmdscale(dismat, k = 2, eig = TRUE)
x <- mds.m1$points[, 1]
y <- mds.m1$points[, 2]
plot(x, y)
text(x + 20, y, label = cnut)

# eigenvalues
xx <- svd(mds.data %*% t(mds.data))
xx$d


# coordinates
xxd <- xx$v %*% sqrt(diag(xx$d))
x1 <- xxd[, 1]
y1 <- xxd[, 2]

plot(x1, y1)
text(x1 + 20, y1, label = cnut)



############################################

install.packages("irlba")
library(irlba)




education.by.readership = matrix(c(5, 18, 19, 12, 3, 7, 46, 29, 40, 7, 2, 20, 39, 49, 16), 
                                 nrow = 5,
                                 dimnames = list(
                                         "Level of education" = c("Some primary", "Primary completed", 
                                                                  "Some secondary", "Secondary completed", 
                                                                  "Some tertiary"),
                                         "Category of readership" = c("Glance", "Fairly thorough", 
                                                                      "Very thorough")))

MyMatrix <- matrix(c(1,3,4,5,0,0,0,
                     1,3,4,5,2,0,1,
                     1,3,4,5,0,0,0,
                     0,0,0,0,4,5,2,
                     0,0,0,0,4,5,2), 
                   nrow = 7,
                   dimnames = list(
                           "Subject" = c("Person1", "Person2", "Person3", "Person4", "Person5", "Person6", "Person7"),
                           "Movie" = c("Matrix", "Alien", "Serenity", "Casablanca", "Amelie")))

        
education.by.readership <- MyMatrix

O = education.by.readership / sum(education.by.readership)
E = rowSums(O) %o% colSums(O)
Z = (O - E) / sqrt(E)

SVD = svd(Z)

print(SVD)


# The singular value decomposition (SVD) has four useful properties. 
# The first is that these two matrices and vector can be "multiplied" 
# together to re-create the original input data, Z.  In the data we 
# started with (Z), we have a value of ([acutal number]-0.064751) in the 5th row, 2nd column. 
# We can work this out from the results of the SVD by multiplying 
# each element of d with the elements of the 5th row of u and the 2nd row v.
# This can be achieved in R using the code:
sum(SVD$d * SVD$u[2, ] * SVD$v[2, ])

# Better yet, if we want to recompute the whole table 
# of numbers at once, we can use a bit of matrix algebra:

SVD$u %*% diag(SVD$d) %*% t(SVD$v)
# Now, at first glance this property may not seem so useful. Indeed, 
# it does not even seem very clever. We started with a table of 
# 15 numbers. Now, we have one vector and two tables, containing 
# a total of 27 numbers. We seem to be going backwards!


###  REDUCING THE DATA

# The second useful property of the SVD relates to the values in d. 
# They are sorted in descending order (ties are possible). Why is this important? 
# Take a look at the last value in d. It is 2.71825390754254E-17. In reality, this 
# is 0 (computers struggle to compute 0 exactly). When recovering the data, we can 
# ignore the last value of d, and also the last column of each of u and v. 
# Their values are multiplied by 0 and thus are irrelevant. Now, we only have 18 numbers 
# to look at. This is still more than the 15 we started with.
# 
# The values of d tell us the relative importance of each of the columns 
# in u and v in describing the original data. We can compute the variance 
# in the original data (Z) that is explained by the columns by first squaring 
# the values in d, and then expressing these as proportions. If you run the 
# following  Rcode, it shows that the first dimension explains 85% of variance in the data.
variance.explained = prop.table(svd(Z)$d^2)

# So, if we are happy to ignore 15% of the information in the original data, 
# we only need to look at the first column in u and the first column in v. 
# Now we have to look at less than half the numbers that we started with.
#
# Halving the number of numbers to consider may not seem like a sufficient benefit. 
# However, the bigger the data set, the bigger the saving. 
# For example, if we had a table with 20 rows and 20 columns, 
# we may only need to look at the first couple of columns, 
# only needing to consider 10% of the number of values that we started with. 
# This is the basic logic of techniques like principle components analysis and 
# correspondence analysis. In addition to reducing the number of values we 
# need to look at, this also allows us to chart the values, which saves more time. 
# There is rarely a good way to chart 20 columns of data, 
# but charting 2 columns is usually straightforward.''''[


##########################################
# http://www.endmemo.com/program/R/svd.php

# svd() function computes the singular-value decomposition
#       of a rectangular matrix.
#
# svd(x, nu = min(n, p), nv = min(n, p), LINPACK = FALSE)
# La.svd(x, nu = min(n, p), nv = min(n, p))

x <- matrix(1:16,4,4)
x
svd(x)

# x: a numeric, logical or complex matrix 
# nu: the number of left singular vectors 
#       to be computed. This must between 
#       0 and n = nrow(x)
# nv: the number of right singular vectors 
#       to be computed. This must be between 
#       0 and p = ncol(x)
# LINPACK: logical. Should LINPACK be used 
#       (for compatibility with R < 1.7.0)? 
#       In this case nu must be 0, nrow(x) or ncol(x)

######################################################################
#
# https://www.youtube.com/watch?v=4DI68P4hicQ
#

MyMatrix <- matrix(c(1,3,4,5,0,0,0,
                     1,3,4,5,2,0,1,
                     1,3,4,5,0,0,0,
                     0,0,0,0,4,5,2,
                     0,0,0,0,4,5,2), 
                   nrow = 7,
                   dimnames = list(
                           "Subject" = c("Person1", "Person2", "Person3", "Person4", "Person5", "Person6", "Person7"),
                           "Movie" = c("Matrix", "Alien", "Serenity", "Casablanca", "Amelie")))


train.irlba <- irlba(t(MyMatrix), nv = 2, maxit = 4)
View(train.irlba$v)


