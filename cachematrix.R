## Function makeCacheMatrix
## Input - A square invertible matrix
## Returns - A list containing functions to
##  1. set the matrix
##  2. get the matrix
##  3. set the inverse - using the solve function
##  4. get the inverse 

makeCacheMatrix <- function(x = matrix()) {
	m<-NULL
	set<-function(y){
	x<<-y ## <<- is used to assign a value to an object in an environment 
	      ## different from the current environment
	m<<-NULL
	}
	get<-function() x
	setmatrix<-function(solve) m<<-solve
	getmatrix <-function() m
	list(set=set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Function cacheSolve
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
	m<-x$getmatrix()
	# if the inverse has already been calculated
	# get it from the cache and skip the costly re-computation
	if(!is.null(m)){
		message("Getting cached data")
		return(m)
	}

	#otherwise, caculate the inverse
	matrix<-x$get()
	m<-solve(matrix, ...)

	#sets the value of the inverse in the cache using the setmatrix function
	x$setmatrix(m)
	m
}
