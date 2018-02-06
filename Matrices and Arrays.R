# Matrices and Arrays as Vectors
# Arrays and matrices (and even lists, in a sense) are actually vectors too, as you'll see. 
# They merely have extra class attributes. For example, matrices have the number of rows and columns
# everything we say about vectors applies to them, too.

# Creating MAtrix

# by using the matrix() function
y <- matrix(c(1,2,3,4,5,6)) # bydefault values are entered initially in first collumn then 2nd collumn
y
y <- matrix(c(1,2,3,4,5,6),nrow=3)
y
y <- matrix(c(1,2,3,4,5,6),nrow=2,byrow=T) # arranging elements by row
y

# Another way to build y is to specify elements individually
y <- matrix(nrow=3,ncol=2)
y[1,1] <- 1    # first row first column
y[2,1] <- 2    # second row first column
y[1,2] <- 3    # 1st row 2nd column
y[2,2] <- 4    # 2nd row 1st column
y[3,1] <- 5
y[3,2] <- 6
y

# Vector In, Matrix Out
x <- 1:8
z12 <- function(z) return(c(z,z^2))
matrix(z12(x),ncol=2)    # inpupt is vector, output is 8 by 2matrix

sapply(x,z12)            # using sapply() (or simply apply). We do get a 2-by-8 matrix, not an 8-by-2 one

# check / find certain elemet in matrix e.g. 2nd row 1st column
y[2,1]       # 2nd row 1st column
y[1,]        # 1st row all columns
y[,1]        # all rows 1st column
y[,1:2]      # all rows 1st and 2nd column


# General Matrix Operations
# These include performing linear algebra operations, matrix indexing, and matrix filtering

# Performing Linear Algebra Operations on Matrices
# matrix multiplication, matrix scalar multiplication, and matrix addition

y+y     # mathematical matrix addition
3*y     # mathematical multiplication of matrix by scalar
y %*% y # mathematical matrix multiplication

# You can also assign values to submatrices:
y
y[c(1,3),] <- matrix(c(1,1,8,12), nrow = 2)
y

# another example of assignment to submatrices:
x <- matrix(nrow=3,ncol=3)
y <- matrix(c(4,5,2,3),nrow=2)
y
x[2:3,2:3] <- y
x

# Negative subscripts, used with vectors to exclude certain elements, work the same way with matrices:
y <- matrix(c(1,2,3,4,5,6),nrow=3)
y[-2,]

# Image Manipulation
library(pixmap)
mtrush1 <- read.pnm("E:\\Data Analytics with RET\\mtrush1.pgm")
mtrush1
plot(mtrush1)
str(mtrush1)

mtrush2 <- mtrush1
mtrush2@grey[84:163,135:177] <- 1
plot(mtrush2)

# Filtering can be done with matrices, just as with vectors.
x <- matrix(c(1,2,3,2,3,4),nrow=3)
x
j <- x[,2]
j
j <- x[,2] >=3
x[j,]

x[x[,2] >= 3,] # direct filtering

#Extended Example: Generating a Covariance Matrix
# Suppose that we are working with an n-variate normal distribution. Our matrix will have n rows and n columns
makecov <- function(rho,n) {
  m <- matrix(nrow=n,ncol=n)
  m <- ifelse(row(m) == col(m),1,rho)
  return(m)
  }

makecov(3,4)

# Applying Functions to Matrix Rows and Columns
# Using the apply() Function : which instructs R to call a user-specified function on each of the rows or each of the columns of a matrix

# apply(m,dimcode,f,fargs) : This is general form of apply for matrices
#m is the matrix.
# dimcode is the dimension, equal to 1 if the function applies to rows or 2 for columns.
#f is the function to be applied.
#fargs is an optional set of arguments to be supplied to f.

z <- matrix(c(1,2,3,4,5,6),nrow=3)
apply(z,2,mean)

f <- function(x) x/c(2,8)
y <- apply(z,1,f)
y

# Finding Outliers
findols <- function(x) {
  findol <- function(xrow) {
   mdn <- median(xrow)
   devs <- abs(xrow-mdn)
   return(which.max(devs))
   }
   return(apply(x,1,findol))
   }

# Adding and Deleting Matrix Rows and Columns

one <- c (1,1,1,1)
one
z <- matrix(c(1,2,3,4,1,1,0,0,1,0,1,0), nrow = 4, ncol = 3)
z

# Analogous operations can be used to change the size of a matrix. For
# instance, the rbind() (row bind) and cbind() (column bind) functions let you
# add rows or columns to a matrix

cbind(one,z)

#You can delete rows or columns by reassignment, too
m <- z[c(1,3),]
m

#Finding the Closest Pair of Vertices in a Graph
#Suppose we need a function that inputs a distance matrix, where the
#element in row i, column j gives the distance between city i and city j and
#outputs the minimum one-hop distance between cities and the pair of cities
#that achieves that minimum. Here's the code for the solution

 # returns the minimum value of d[i,j], i != j, and the row/col attaining
 # that minimum, for square symmetric matrix d; no special policy on ties
 mind <- function(d) {
 n <- nrow(d)
 # add a column to identify row number for apply()
 dd <- cbind(d,1:n)
 wmins <- apply(dd[-n,],1,imin)
 # wmins will be 2xn, 1st row being indices and 2nd being values
 i <- which.min(wmins[2,])
 j <- wmins[1,i]
 return(c(d[i,j],i,j))
 }

 # finds the location, value of the minimum in a row x
 imin <- function(x) {
   lx <- length(x)
   i <- x[lx] # original row number
   j <- which.min(x[(i+1):(lx-1)])
   k <- i+j
   return(c(k,x[k]))
 }
 
 
q <- matrix(c(0,12,13,8,20,12,0,15,28,88,12,15,0,6,9,8,28,6,0,33,20,88,9,33,0), nrow = 5, ncol = 5)
q  
mind(q)

# Vector/Matrix Distinction
z <- matrix(1:8,nrow=4)
z 
length(z) # As z is still a vector, we can query its length:
class(z)  # But as a matrix, z is a bit more than a vector:
attributes(z) 
dim(z)    # dimensions of matrix
nrow(z)   # no. of rows
ncol(z)   # no. of columns

# Avoiding Unintended Dimension Reduction
# You may find that your code works fine in general but fails in a special case. For instance, suppose that your code
# extracts a submatrix from a given matrix and then does some matrix operations
# on the submatrix. If the submatrix has only one row, R will make it a vector, which could ruin your computation.

r <- z[2,, drop=FALSE] # way to suppress this dimension reduction: the drop argument
r
dim(r)  # Now r is a 1-by-2 matrix, not a two-element vector.

# For these reasons, you may find it useful to routinely include the drop=FALSE argument in all your matrix code.

# If you have a vector that you wish to be treated as a matrix, you can use the as.matrix() function, as follows
z <- c(1,2,3)
v <- as.matrix(z)
attributes(v)

# Naming Matrix Rows and Columns
z <- matrix(c(1,2,3,4), nrow = 2, ncol = 2)
colnames(z)
colnames(z) <- c("a", "b")
rownames(z) <- c("x", "y")
z


# Higher Dimensional Arrays
# suppose we also have data taken at different times, one data
# point per person per variable per time. Time then becomes the third dimension,
# in addition to rows and columns

# consider students and test scores. Say each test
# consists of two parts, so we record two scores for a student for each test

firsttest <- matrix(c(46,21,50,30,25,50), nrow = 3 , ncol = 2) # data for the first test
secondtest <- matrix(c(46,41,50,43,35,50), nrow = 3 , ncol = 2) # data for second test

# let's put both tests into one data structure, which we'll name tests.
# We'll arrange it to have two "layers"-one layer per test-with three rows

tests <- array(data=c(firsttest,secondtest),dim=c(3,2,2))
tests        # R's print function for arrays displays the data layer by layer:
attributes(tests)    # result is 3 rows, 2 column and 2 dimension (layers)
