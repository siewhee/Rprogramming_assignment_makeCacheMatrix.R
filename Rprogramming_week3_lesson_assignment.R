## w3 r programming assignment 

makeCacheMatrix <- function(x =matrix()){
  inver  <- NULL
  set    <- function(y){
    x     <<- y
    inver <<- NULL
  }
  get <- function(){x} 
  setInverse <- function(inverse) {inver <<- inverse}
  getInverse <- function(){inver}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
  inver <- x$getInverse()
  if(!is.null(inver)) {
    message("getting cached data")
    return(inver)
  }
  mat   <- x$get()
  inver <- solve(mat, ...)  
  x$setInverse(inver)
  inver
}
