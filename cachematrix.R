## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
	get<-function() x
	setInverse<-function(inverse) inv<<-inverse
	getInverse<-function() inv
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	
	inv<-x$getInverse
	# if the inverse matrix is stored in cache, then return inverse matrix from the cache
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	mat<-x$getInverse() # assign the matrix 'x' into another matrix named 'mat'
	inv<-solve(mat,...) # solve for inverse
	x$setInverse(inv) # cache the inverse matrix
	inv
}
