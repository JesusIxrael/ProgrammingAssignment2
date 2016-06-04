## Put comments here that give an overall description of what your
## functions do

## This function generates a special matrix, then gets the value of that matrix. 
##Following it set the value of the inverse of the matrix 
##and finally it gets the value of the inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
	set<- function (y) {
		x<<-y
		inv<<-NULL
	}
	get<- function()x 
	SetInverse<- function (inverse) inv <<-inverse
	GetInverse<- function () inv
	list 	(set=set,
		 get=get,
		SetInverse=SetInverse,
		GetInverse=GetInverse)
}


## The following function computes the inverse of the special matrix created before, 
##if the inverse of that matrix has been calculated before, the function get that from the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<- x$GetInverse()
	if (!is.null(inv)) {
			message( “Geting Cache Data”)
			return(inv)
	}
	mat <- x$get()
	inv<-  solve(mat,…)
	x$setInverse(inv)
	inv
}
