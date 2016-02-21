## Cousera R Programming: Assignment 2
## Objective: Write 2 functions to cache the inverse of 
## a matrix

## Assumption: Matrix is invertible

## Create a special "matrix" object that can cache its inverse
## This function is essentially the same as the makeVector function in the 
## assignment description
makeCacheMatrix <- function(x = matrix()) {
    
    my_inv <- NULL
    set <- function(y) {
        x <<- y
        my_inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) my_inv <<- inverse
    getInverse <- function() my_inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


## Computes the inverse "special" matrix if not already in cache else
## retrieve from cache
## This function is essentially the same as the cachemean function in the assignment
## description except to replace built-in function mean with solve to calculate the
## actual solution

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv   
    
}

## Test the functions with example outputs
## Source the file if you want to load the file

## source(cachematrix.R)

## create a matrix
# > my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

## Print out the matrix
# > my_matrix$get()
# [,1] [,2]
# [1,]    1    3
# [2,]    2    4

## show that the inverse not calculated
# > my_matrix$getInverse()
# NULL

## Solve for the inverse
# > cacheSolve(my_matrix)
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## show that inverse is calculated
# > my_matrix$getInverse()
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

## solve matrix again to show that result is obtained from cache
# > cacheSolve(my_matrix)
# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
