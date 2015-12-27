## This function will make a matrix object that can have the inverse cached


makeCacheMatrix <- function(x = matrix()) {
      xinv <- NULL 
	# this is where the result of inversion is stored
      
      set <- function(y) {
	  	x <<- y
	 	xinv <<- NULL # it also initialises xinv to null
        }

      get <- function() {x} # return the input matrix
      setInv <- function(inv) {xinv <<- inv} # set the inversed matrix
      getInv <- function() {xinv} # return the inversed matrix
      
      list(set = set, get = get,setInv = setInv, getInv = getInv)
	# return a list that contains these functions
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      m <- x$getInv() # get the inversed matrix from object x
      # it will be null if uncalculated, remember the first line "xinv <- NULL" in the previous function
      
	if(!is.null(m)) { # if there is an inversion result cached already
		message("retrieving cached data")
		return(m) 
      }
      
      data <- x$get() # if not, we do x$get to get the matrix object
      m <- solve(data) # we solve it
      x$setInv(m) # we then set it to the object
      print(m) # print the solved result
}
