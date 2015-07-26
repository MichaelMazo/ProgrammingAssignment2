## Put comments here that give an overall description of what your
## functions do

## Below are defined 2 functions: "makeCacheMatrix" and "cacheSolve".
## "makeCacheMatrix" creates a special "matrix" object containg a matrix, its inverse matrix, and 
## 4 functions which set and get values of the 2 matrices.
## "cachSolve" takes a "matrix" and returns the inverse. If the inverse 
## is already calculated and stored in the "matrix" "cacheSolve" returns the strored value.

## Write a short comment describing this function
## "MakeCacheMatrix takes
## a numeric or complex matrix and creates a "matrix" which is a list of 4 functions:
## 1. Set the value of the matrix and set the value of the inverse euqal "NULL".
## 2. Set the value of the inverse
## 3. Get the value of the matrix
## 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Write a short comment describing this function
## "casheSolve" takes a "matrix", checks whether the inverse is already calculated. If so,
## it returns the inverse from cache and skips the calculation. Otherwise, it calculates
## the inverse, put it into cache using 'setinv" and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
