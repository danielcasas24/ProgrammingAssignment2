## Pair of functions tO cache the inverse of a matrix


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  ## Return a matrix that is the inverse of 'x'
}

# R program to find inverse of a Matrix 

# Create 3 different vectors 
# using combine method. 
a1 <- c(3, 2, 5) 
a2 <- c(2, 3, 2) 
a3 <- c(5, 2, 4) 
A <- rbind(a1, a2, a3) 

# print the original matrix 
print(A) 

# Use the solve() function  
# to calculate the inverse. 
T1 <- solve(A) 

# print the inverse of the matrix. 
print(T1) 

T2 <- cacheSolve(makeCacheMatrix(A))

print(T2)
