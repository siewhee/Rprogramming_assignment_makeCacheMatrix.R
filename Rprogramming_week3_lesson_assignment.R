## w3 r programming assignment 

makeCacheMatrix <- function(x =matrix()){
  #initialising inverse as null
  inver  <- NULL
  set    <- function(y){
    x     <<- y
    inver <<- NULL
  }
  # to get x function
  get <- function(){x} 
  setInverse <- function(inverse) {inver <<- inverse}
  #to get inverse
  getInverse <- function(){inver}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# to get cache
cacheSolve <- function(x, ...){
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    # to return inverse
    return(inver)
  }
  mat   <- x$get()
  inver <- solve(mat, ...)  
  x$setInverse(inver)
  inver
}
