## Purpose of these two fucntions is to cache result of reversing a matrix in R
## As matrix reverse operations are resource intensive this is one way to obtain faster results
## Usage:
## mat<-matrix(c(1,0,0,1),2)
## cacheSolve(makeCacheMatrix(mat))
## Using same function next time will result in faster execution as it avoides call to solve function
## cacheSolve(makeCacheMatrix(mat))

## makeCacheMatrix function holds results of solve function after one execution

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve determines if makeCacheMatrix already have cached results before executing solve function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached inverse matrix")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        return(inv_x)
    }
}
