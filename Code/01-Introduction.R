# R Introduction

# Calculator ----------------------------------------------------

2 + 5

4 * 8

2^3

sqrt(4)

exp(1)

log(exp(1))

# Help search with "?"
?log		

log(10)
log(10, 10)



# Data objects -----------------------------------------------


# Assignment operators
x = 8
x
x <- 7
x


x + 5
2 * x



# Vectors
y <- c(1, 2, 3, 4, 5)
y

z <- c(6:10)
z

y + z
y * z

z[2]			    # second element of z
z[-2]			    # all elements of z except the second
z[1:3]		    # first, second, and third elements of z
z <- c(z, 11)	# append value "11" to z

log(y)
sum(y)
which(z > 7)




# Matrices
matrix(data = 0, nrow = 4, ncol = 5)
matrix(0, 4, 5)

a <- matrix(1, 2, 2)
b <- matrix(1:4, 2, 2)

a + b
a * b			# elementwise multiplication
a %*% b		# traditional matrix multiplication

b[1, 2]		# element in first row second column
b[1, ]		# first row
b[, 1]		# first column

c <- cbind(a, b)
c
d <- rbind(a, b)
d




# Data frames
v1 <- c("Liz", "Jack", "Tracy")
v2 <- c(24, 37, 19)
v3 <- c(1, 0, 0)

dat <- data.frame(name = v1, age = v2, gender = v3)
dat
dat[, 1]
dat$name
dat[, c(1, 3)]
dat[1,]
dat[c(1, 3), ]

attach(dat)
name
detach(dat)
name


fix(dat)



# Data import/export ---------------------------------------------


?read.table
?read.csv


#data <- read.table(file = "filename.whatever", ...) 
# full file path not needed if file is in working directory

wine <- read.csv(file = "wine.csv")
#wine <- read.csv(file.choose())
str(wine)

gene <- read.csv(file = "https://raw.githubusercontent.com/tylerlewiscook/UCO-Data-Mining-Stat-Learning/master/Data/gene.csv")

save(b, file = "favoriteMatrix.RData")
rm(b)
load(file = "favoriteMatrix.RData")
write.csv(b, file = "favoriteData.csv")



# Packages -------------------------------------------------------


install.packages("ISLR")
library(ISLR)


