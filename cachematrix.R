# Excuse me for my English. I'm french student!!!

# Matrix inversion is expensive in computational time. For better performance,
# the inverse of the matrix is cached (no need to recalculate it each time)
# The finctions makeCacheMatrix.R and cacheResolve.R are used to do this

# set matrix value
# get matrix value
# set inverse matrix value
# get inverse matrix value

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  fixer <- function(y) {
    x <<- y
    xInv <<- NULL
  }
  obtenir <- function() x
  fixerInverse <- function(inverse) xInv <<- inverse
  obtenirInverse <- function() xInv
  list(fixer=fixer, obtenir=obtenir, fixerInverse=fixerInverse, obtenirInverse=obtenirInverse)
}


# cacheSolve.R return matrix inverse value
# It first checks if the inverse of the matrix has already been computed and is in the
# cache. If so, it retrieves the value and skips the calculation.
# Otherwise, it calculates the inverse of the matrix and stores the value in the cache.
cacheSolve <- function(x, ...) {
 inv <- x$obtenirInverse()
  if(!is.null(inv)) {
    message("Obtenir les donnees en cache.")
    return(inv)
  }
  donnee <- x$obtenir()
  inv <- solve(donnee)
  x$fixerInverse(inv)
  inv
}
