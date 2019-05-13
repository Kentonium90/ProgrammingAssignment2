## "makeCacheMatrix" calculates and stores the inverse of an input matrix. 
## "cacheSolve" accesses this stored inverted matrix without recalculating
## it--which can save time in applications that might re-use the inverted 
## multiple times. When the user needs to invert a new matrix, the function
## replaces the previously stored result with the new one. 

## makeCacheMatrix permits the user to calculate and store the inverse of 
## an input matrix. 

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
	x <<- y
	m <<- NULL
	}
	get <- function()x
	setmatrix <- function(solve) m <<- solve
	getmatrix <- function() m 
	list(set = set, get = get, setmatrix = setmatrix, getmatrix = getmatrix)
}


## cacheSolve accesses the inverse matrix stored by the previous function.
## If the user wishes to solve a new matrix, this can be reset by re-setting
## "myMatrix_object", which calculates and stores a new matrix inverse. 

cacheSolve <- function(x, ...) {
	m <- x$getmatrix()
	if(!is.null(m)){
	message("getting cached data")
	return(m)
	}
	data <- x$get()
	m <- solve(data, ...)
	x$setmatrix(m)
	m
}

