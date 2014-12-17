## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
######################################################################
## makeCacheMatrix(x), argument an inversible matrix x, 
## implements setters and getters for matrix and inverse matrix
## see details in comments interveaved with code
##
## usage example:
## A = makeCacheMatrix( matrix(1:4, 2, 2) )
## call A$get() to see matrix A
## call A$getinv() to see inverse of A -- is still NULL
## call cacheSolve(A), inverse of A is computed
## A$getinv() now returns inverse of A
## cacheSolve(A) now returns message and reads inverse from cache
######################################################################
 
makeCacheMatrix <- function(x = matrix()) {
	## initialize inverse matrix 
	inv = NULL
	
	## setter function: set matrix x to argument y and inverse of x to NULL
        ## -- use <<- operator
	set = function(y) {
		x <<- y
                inv <<- NULL
	}

	## getter function: returns matrix x
	get = function() x
    
        ## setter for inverse matrix, sets inverse matrix to argument -- use <<- operator
	setinv = function(inverse) inv <<- inverse

	## getter for inverse matrix, returns inverse matrix
	getinv = function() inv
	
	## function returns list of setters and getters
	list(set = set, get = get, setinv = setinv, getinv = getinv) 
}


## Write a short comment describing this function
###############################################################################
## cacheSolve(A), argument A, a matrix object created with makecacheMatrix 
## call getter for inverse of A
## if this returns not NULL, inverse has already been computed and stored
## in cache, so return inverse
## otherwise compute inverse with function solve from R-library and return inverse of A
###############################################################################
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        ## call getter for inverse of our matrix x --- may still be NULL
	inv = x$getinv()
	
	## if inverse is not NULL, hence has already been computed, nothing to do just give a message and return it
	if (!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}

	## if we are here inverse has not been computed
	## get the matrix object
	data = x$get()

	## compute inverse matrix (we can assume that it exists)
	inv = solve(data, ...)
	
	## set inverse matrix to result and return it
	x$setinv(inv)
	inv
}
