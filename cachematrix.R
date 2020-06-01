## The 2 functions for Prog. Assignment 2 (Week 3 of 'R Programming')
#
## To run, after sourcing 'cachematrix.R' try steps 1-6 from R console:
#(1) z <- (Supply an invertible square matrix here)
#(2) a <- makeCacheMatrix(z)
#(3) a$set(z)
#(4) cacheSolve(a) #this command gives the inverse of the matrix z
#(5) cacheSolve(a) #same answer, but labeled: "getting cached data"
#(6) a$getsolve()  #gives the same inverse
#
## Function 1: 'makeCacheMatrix'
## ----------------------------
## This creates a matrix object that can cache its inverse
## 'x' is a non-singular square matrix of any dimension.
##
## Initialize 'inv_x' and 'y' as matrices of the same dimensions as 'x'
makeCacheMatrix <- function(x = matrix()) {
    inv_x <- matrix(data=NA, nrow=dim(x)[1], ncol=dim(x)[2])
    y     <- matrix(data=NA, nrow=dim(x)[1], ncol=dim(x)[2])
#
## Fn 'set' assigns values to x and inv_x in the parent environment
## inv_x will later hold the inverted matrix
    set <- function(y) {
        x <<- y
        inv_x <<- NULL }
##    
    get <- function() x
    setsolve <- function(solve) inv_x <<- solve
    getsolve <- function() inv_x
#
# Return the 3 functions defined above as an R list
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
 }
## ----------------------------------------
## Function 2: 'cacheSolve'. This function
## -----------------------
## computes the inverse of the matrix returned by 'makeCacheMatrix',
## checks if the inverse has already been calculated, and if so,
## fetches the inverse from the cache instead of recalculating it,
## returns the inverse, and stops the function.
## If the inverse hasn't been computed, cacheSolve computes it.
##
cacheSolve <- function(x,...) {
# Note: This 'x' is not the 'x' in the 'makeCacheMatrix' fn declaration!
    inv_x <- x$getsolve()
    if(!is.null(inv_x)) {
        message("     GETTING CACHED INVERSE OF MATRIX!")
        return(inv_x)   }
    data <- x$get()
    inv_x <- solve(data, ...)
    x$setsolve(inv_x)
    inv_x
}