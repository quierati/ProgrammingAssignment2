## Put comments here that give an overall description of what your
## functions do

## This function create cache of inverse matrix

makeCacheMatrix  <- function(x = matrix()) {
  cache <- NULL
  # put matrix in cache
  set <- function(tmp) {
    x <<- tmp
    cache <<- NULL
  }
  # return matrix
  get <- function() x
  # put cache
  setcache <- function(inverse) cache <<- inverse
  # return cache 
  getcache <- function() cache
  # return methods available
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

## This function calculate inverse of matrix and save in cache if not exists

cacheSolve <- function(x = makeCacheMatrix(), ...) {
  # find matrix in cache
  cache <- x$getcache()
  # if cache exists
  if(!is.null(cache)) {
    message("getting cached data")
    # return inverse of matrix from cache
    return(cache)
  }
  # fetch matrix data via get function
  data <- x$get()
  inverse <- solve(data, ...)
  # put inverse of matrix via setcache function 
  x$setcache(inverse)
  ## Return a matrix that is the inverse of 'x'
  inverse
}
