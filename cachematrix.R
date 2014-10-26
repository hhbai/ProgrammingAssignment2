## Author: Hanwen (Evan) Bai
## Date: 10-26-2014
## Programming Assignment 2, R Programming, Coursera
##
## matrix inversion is a costly computation.
## create a pair of functions that cache the inverse of a matrix.
## -------------------------------------------------------------- ##

## goal: create a special matrix that can cache its inverse
## input: a matrix object
## output: a list
## 	(1) setData: reset the input matrix
##	(2) getData: retrieve the cached input matrix
##	(3) setInverse: cache the inverse matrix
##	(4) getInverse: retrieve the cached inverse matrix
makeCacheMatrix <- function(x=matrix()) {
	## initialize inverse matrix
	mat <- x
	inv <- NULL
    
	## set input matrix
	setData <- function (mat2) {
		mat <<- mat2
		inv <<- NULL
	}
	
	## retrieve input matrix
	getData <- function() {
		return (mat)
	}
	
	## set inverse matrix
	setInverse <- function (inverse) {
		inv <<- inverse
	}
	
	## retrieve inverse matrix
	getInverse <- function() {
		return (inv)
	}
	
	## output object
	x <- list (setData = setData, getData = getData, setInverse = setInverse, getInverse = getInverse)
	return (x)
}


## goal: compute/retrieve the inverse of a matrix
## input: a special matrix object that can cache its inverse
## output: the inverse of the matrix
cacheSolve <- function (x, ...) {
	## retrieve cached inverse matrix
	inv <- x$getInverse()
	
	## check cached inverse matrix
	if(!is.null(inv)) {
		message ("getting cached data")
		return (inv)
	}
	
	## compute inverse matrix
	mat <- x$getData()
	inv <- solve(mat, ...)
	x$setInverse(inv)
	return (inv)
}

