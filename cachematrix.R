# Coursera: R Programming (rprog-031)
# Programming Assignment 2
# Author: Brandon Hoeft
# Date: August 22, 2015
# Objective: Write a function that will store and cache values and a function that will
# calculate the inverse of a square matrix and cache those values in the original function. For 
# this assignment, assume that the matrix supplied is always invertible (don't need to program to
# account for this)


# Function 1: makeCacheMatrix ####

# Definition: this function creates a special matrix object that contains a list of 4 
# separate functions:
# set <- sets a new matrix to overwrite the prior or original x matrix in the argument
# get <- retrieves the original matrix or the most recently set matrix 
# set.matrix.inverse <- assigns the inverse matrix obtained by running the next function,
# cachSolve, and stores the inverse matrix in this variable.
# get.matrix.inverse <- prints the last cached inverse matrix stored from cacheSolve

makeCacheMatrix <- function(x = matrix()) {
    if (nrow(x) != ncol(x)) {
        message("Warning: This is not a Square Matrix. You will not be able to
                compute the inverse using cacheSolve function with this matrix.")
    }
    
    matrix.inverse <- NULL
    set <- function(y) {
        x <<- y
        matrix.inverse <<- NULL
        if (nrow(y) != ncol(y)) {
            message("Warning: This is not a Square Matrix. You will not be able to
                compute the inverse using cacheSolve function with this matrix.")
        }
    }
    get <- function () { x }
    set.matrix.inverse <- function(cacheSolve.output) {matrix.inverse <<- cacheSolve.output} 
    get.matrix.inverse <- function() {matrix.inverse}
    
    list(set = set, get = get, 
         set.matrix.inverse = set.matrix.inverse,
         get.matrix.inverse = get.matrix.inverse) # the list is the output
}


# Function 2: cacheSolve function ####

# Definition: its argument is the object storing makeCacheMatrix; the function returns a matrix
# that is the inverse of the 'x' matrix defined in makeCacheMatrix. However, if the inverse of the
# given 'x' matrix was already computed using solve(), then this function returns that cached 
# value from makeCacheMatrix. 

cacheSolve <- function(x, ...) {
    if(nrow(x$get()) != ncol(x$get())) {    # issue warning for non-square matrix
        message("WARNING: The matrix must be square in order to compute its inverse.")
        break                               # if TRUE, function stops here.
    }
    
    matrix.inverse <- x$get.matrix.inverse() # pull the last cached inverse matrix
    if (!is.null(matrix.inverse)) {
        message ("Pulling the last cached inverse matrix!")
        print(matrix.inverse)
    } else {                                #if no cached inverse, then solve system of equations
        new.matrix <- x$get()
        solution <- solve(new.matrix)
        x$set.matrix.inverse(solution)
        
        solution # prints solution
    }
}

# Test the functions out using a 3x3, a 1x9 matrix, and a 2 x 2 matrix ####
temp <- makeCacheMatrix(matrix(c(1, 5, 23, 53, 11, 7, 22, 25, 5), nrow = 3, ncol = 3)) # square
temp2 <- makeCacheMatrix(matrix(c(1, 5, 23, 53, 11, 7, 22, 25, 5))) 

temp$get() # starts out with matrix defined in the argument
temp$set() # have not fed any new matrix to overwrite the original in temp.
temp$set.matrix.inverse() # should not work, since cacheSolve.output has not been computed yet
temp$get.matrix.inverse() # starts out NULL
cacheSolve(temp) # print the inverse of originally defined matrix 
temp$get.matrix.inverse() # prints the now cached inverse matrix from above

temp$set(matrix(c(1, 5, 23, 53, 11, 7, 22, 25, 5))) # 1 x 9 matrix
cacheSolve(temp)

temp$set(matrix(c(27, 18, 91, 1444), nrow = 2 , ncol =2))
cacheSolve(temp)