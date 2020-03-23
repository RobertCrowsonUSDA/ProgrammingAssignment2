## MakecacheAMtrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

# <<- operator looks back in enclosing environments for an environment that contains the symbol
# yes it may have to many comments for you, but lexical scoping is odd to me

## a special object htat has a matrix and can have its inverse
makeCacheMatrix <- function(x = matrix()) {
  # each time set it to null
  inv <- NULL

  set <- function(y) {
    x <<- y # look for X in the environment and set it to Y
    inv <<- NULL # look for inv in the environment and set it to null
  }
  
  get <- function() x  #x is a free variable in get()
  
  setInverse <- function(inverse) inv <<- inverse #inv is a free variable which is inverse in the environment
  
  getInverse <- function() inv #inv is a free variable
  
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse)  # just a normal list

}


## inverse of special matrix from makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # if it exists, get the inverse Or get the subset of x which is getInverse()
  inv <- x$getInverse()
  # return inv is it exists end function
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # nothing unusual here
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv  
  

}
