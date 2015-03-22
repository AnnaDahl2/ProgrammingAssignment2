## Created as an assignment in the "R programming" Coursera course 
## (https://class.coursera.org/rprog-012/)

## The learning goal of this assigment is to understand lexical scoping and the <<- 
## operator, which can modify variables in parent levels. This allows us to create
## functions like these, in which we may "cache" the results of potentially time-consuming
## computations - in this case the inverse of a matrix.

## The code may be tested as follows:
## 1) create an inversible matrix
## mymatrix <- matrix(data = c(4,7,2,6), nrow = 2, ncol = 2)
## 2) create the "matrix" object that can cache its inverse:
## myspecialmatrix <- makeCacheMatrix(mymatrix)
## 3) compute the inverse of the matrix:
## cacheSolve(myspecialmatrix)
## 4) confirm that the inverted matrix is returned from "cache" when
## the function is run a second time:
## cacheSolve(myspecialmatrix)
## (Will return the line "getting cached data" along with the matrix)



## The makeCacheMatrix function creates a special "matrix" object that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has
## not been changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
        inv <- x$getinverse()
        
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
