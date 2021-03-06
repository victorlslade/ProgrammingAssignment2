## Programming Assignment 2
## Victor Slade
## 5/25/14
## These functions implement cacheing the inverse of a matrix.


#makeCacheMatrix accepts matrix as input, creates a list containing functions to set the matrix, get the matrix, set the inverse
#of the matrix and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
                setInv = setInv,
                getInv = getInv)
}



#The following function calculates the inverse of matrix created with the above function. 
#It checks if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the 
#computation. Otherwise it sets the value of the inverse in the cache via
#the setinverse function.
cacheSolve <- function(x, ...) {
        m <- x$getInv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        m
}
