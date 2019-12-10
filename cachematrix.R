## This function creates a square matrix to catch its inverse
## expecting that the matrix inputted is always invertible
## The following function would then result to the inverse of the matrix returned by makeCacheMatrix
## Once the inverse is calculated, the cachesolve can retrieve the inverse from the cache

## This function first turns a square matrix to catch its inverse and saved in the system

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
	        set <- function(y) {
		        x <<- y
		        inv <<- NULL
	        }
	get <- function()x
	setinv <- function(inverse)inv <<- inverse
	getinv <- function()inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function below will return the result (inverse of matrix) by makeCacheMatrix above
## such that the cachesolve can retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
	if(!is.null(inv)){
		message("inverse is called")
		return (inv)
	}
	data <- x$get()
	inv <- solve(data,...)
	x$setinv(inv)
	return(inv)
}
