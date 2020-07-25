## The fuctions below are used for computing the inverse of a matrix but storing
## the result in cache for future requests of the same matrix.

## The "makeCacheMatrix" fuction set the enviroment where the varibles are stored
## and set the fuctions that return the chached Matrix and theses fuctions are 
## returned in a list

makeCacheMatrix <- function(x = matrix()) {
  m_inv <- NULL
  m_set <- function(y){
    x <<- y
    m_inv <<- NULL
  }
  m_get <- function() x
  set_inv <- function(inv) m_inv <<- inv
  get_inv <- function() m_inv
  list(m_set = m_set,
       m_get = m_get,
       set_inv = set_inv,
       get_inv = get_inv)
}


## The "cacheSolve" fuction ueses a lits in its argument and look for the inverse
## of the matrix and if the inverse does not exist, this fuction compute it and
## store it in the original list

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m_inv <- x$get_inv()
  if(!is.null(m_inv)){
    message("getting cached data")
    return(m_inv)
  }
  mat <- x$m_get()
  m_inv <- solve(mat)
  x$set_inv(m_inv)
  m_inv
}
