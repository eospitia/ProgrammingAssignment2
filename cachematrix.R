## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inversa <- NULL #variable con valor NULO
  set <- function(y) {
    x <<- y
    inversa <<- NULL
  }
  get <- function() x #funcion para obtener matrix x
  setinversa <- function(inversa) inversa <<- inversa
  getinversa <- function() inversa # funcion para obtener la inversa de la matrix
  list(set = set, get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

x <-makeCacheMatrix(matrix(1:4,nrow= 2,ncol= 2))
print(x)
x$get()

## cacheSolve computes the inverse of "matrix"

cacheSolve <- function(x, ...) { #obtener los datos en cache
  inversa <- x$getinversa()
  if(!is.null(inversa)) { #revisar si inversa es nula
    message("getting cached data")
    return(inversa)
  }
  data <- x$get()
  inversa <- solve(data, ...) #solve(X) devuelve su inversa de la matriz
  x$setinversa(inversa) 
  inversa
}

cacheSolve(x) ## Return a matrix that is the inverse of 'x'
