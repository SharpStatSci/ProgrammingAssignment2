# cachematrix.R - Jesse Sharp June 19 2015
# Programming Assignment 2 - Scoping and Efficiency
# Assignment: Caching the Inverse of a Matrix
# My functions follow the given samples for vectors/means
# All input matrices are assumed to be square

# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# this will save processing time by returning the inverse if it has already been computed vs.
# recomputing the value. If the inverse is not available then it gets computed now.

makeCacheMatrix <- function(x = matrix()) {

    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }

    get <- function() x
    setmatrix <- function(solve) m<<- solve
    getmatrix <- function() m

    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)

}


#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix
#above. If the inverse has already been calculated (and the matrix has not changed), then the
#cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {

    m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)

    }

    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}



# here are tests and results - remove ## to run tests
# create a few invertible matrices and test
## m1 <- diag(4)
## m2 <- matrix(1:4, 2, 2)


## y <- makeCacheMatrix(m1)
## cacheSolve(y)

#> cacheSolve(y)
#     [,1] [,2]  [,3] [,4]
#[1,]    1    0    0    0
#[2,]    0    1    0    0
#[3,]    0    0    1    0
#[4,]    0    0    0    1



## cacheSolve(makeCacheMatrix(m2))

#> cacheSolve(makeCacheMatrix(m2))
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5

# end of file