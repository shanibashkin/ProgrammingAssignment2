## General note
## ============
## The functions enables a store in cache functionality for
## a "special" matrix and its inverse.
## "Special" matrix is a square invertible matrix
## Assumption: the matrix supplied is always invertible


## makeCacheMatrix
## ===============
## Creates a special "matrix" object that can cache its
## supplied matrix and its inverse.
## Relevant operations of the "matrix" object that can be
## consumed: SET (for a matrix), GET (a matrix),
## SET_SOLVE (set an inverse), GET (get an inverse)
makeCacheMatrix <- function(mat = matrix()) {
  
  mat_solve <- NULL
  
  set <- function(new_matix) {
    mat <<- new_matix
    mat_solve <<- NULL
  }
  get <- function() mat
  
  set_solve <- function(solve) mat_solve <<- solve
  get_solve <- function() mat_solve
  
  list(set = set, get = get,
       set_solve = set_solve,
       get_solve = get_solve)
}


## cacheSolve
## ==========
## Computes the inverse of the "special" matrix returned by 
## makeCacheMatrix above. If inverse has already been 
## calculated, then retrieve inverse from the cache.
## Otherwise, the function calculates inverse of the 
## "special" matrix (by Solve function) and store it in the
## cache for next time.
cacheSolve <- function(mat, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat_solve <- mat$get_solve()
  if(!is.null(mat_solve)) {
    message("getting cached data")
  }
  else {
    data <- mat$get()
    mat_solve <- solve(data, ...)
    mat$set_solve(mat_solve)
  }
  mat_solve
}