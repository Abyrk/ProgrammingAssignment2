## These functions work together to reduce the processing
## time requires to compute the inverse of a square matrix.
## It assumes that the supplied matrix is invertible.

## Example:
## x<- matrix(rnorm(25),nrow=5)
## cachex<- makeCacheMatrix(x)
## cachex$get()
## cacheSolve(cachex)
## cacheSolve(cachex)

## makeCacheMatrix creates a special "matrix" oject that can cache
## the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
        myInv <- NULL
        set <- function(y){
                x <<- y
                myInv <<-NULL
        }
        get <- function() x
        setInv <- function(solve) myInv <<- solve
        getInv <- function() myInv
        list (set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve computes the special "matrix" returned by the
## makeCacheMatrix function. If the inverse has already been
## calculated the stored result of makeCacheMatrix is used instead.

cacheSolve <- function(x, ...) {
        myInv <-x$getInv()
        if(!is.null(myInv)){
                message("getting cached data")
                return(myInv)
        }
        data <- x$get()
        myInv <- solve(data)
        x$setInv(myInv)
        myInv
}
