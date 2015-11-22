## My first function take a squared matrix and it store this matrix
## in cache and when the second function needs show the inverse matrix
## then it uses the first function to know if the calculation was done
## before, if the answer was not then it calculate the inverse matrix 
## and the first function stores the inverse matrix for a next time.

## Comment describing this function:
## This function save a squared matrix that the user give, and store
## the necesary steps to calculate the inverse matrix when it can be 
## necesary. All of it is save in a list object.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse = function(solve) m <<- solve
        getinverse = function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Comment describing this function:
## This function take as argument the matrix in the first function and
## request to the list object the element getinverse to know if the content
## is a non null element, if the answer is true then it recover the inverse
## matrix in the list object generated in the first function. if the answer
## is negative then data take values of the original matrix stored in the list
## object and calculate the inverse matrix and send it to the first function
## to store the inverse for next time.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m                                       ## Return a matrix that is the inverse of 'x'
}
