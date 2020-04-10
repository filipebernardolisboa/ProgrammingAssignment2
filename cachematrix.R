## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        mInv <- NULL # initiates the mInv variable
        set <- function(y){
                x <<- y # in case the makeCacheMatrix$set() gets called assigns a new value to x
                mInv <<- NULL # removes any mInv that has been stored in memory by a previous call of the cacheSolve() function
        }
        get <- function() x # defines behaviour of get function
        setinverse <- function(inverse) mInv <<- inverse
        getinverse <- function() mInv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse= getinverse) # returns a list of four functions 
}

# The following function returns the inverse of the matrix. It first checks if
# the inverse has already been computed. If so, it gets the result and skips the
# computation. If not, it computes the inverse, sets the value in the cache via
# setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mInv <- x$getinverse() # gets the inverse of a matrix if it exists already
        if(!is.null(mInv) && mInv == solve(data, ...)) { # evaluates if the calculation was already done
                message("getting cached inversed matrix")
                return(mInv) # returns previously calculated matrix
        } # otherwise...
        data <- x$get() # gets data from the input
        mInv <- solve(data, ...) # calculates the inverse
        x$setinverse(mInv) # a "setter" for the calculated value 
        mInv
}