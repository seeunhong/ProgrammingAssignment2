# This function(makeCacheMatrix) requires a matrix as an input
# Set the value of the matrix and get the value of the matrix
# And then set the inverse Matrix and get the inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
        inverseMatrix <- NULL
        
        #set the value of the Matrix
        setMatrix <- function(y) {
                x <<- y
                inverseMatrix <<- NULL
        }
        #get the value of the Matrix
        getMatrix <- function() x
        #set the value of the invertible matrix
        setInverse <- function(inverse) inverseMatrix <<- inverse
        #get the value of the invertible matrix
        getInverse <- function() inverseMatrix                     
        list(setMatrix = setMatrix, getMatrix = getMatrix,
             setInverse = setInverse, getInverse = getInverse)
        
}



# Second function(cacheSolve) takes the output of the previous matrix(makeCacheMatrix)
# And put it in as a input
# After that it checks if there is a value of inverse matrix in makeCacheMatrix
# If it is empty, it gets and sets the inverse matrix by using the solve function
# And the cached object

cacheSolve <- function(x, ...) {
        inverseMatrix <- x$getInverse()
        if(!is.null(inverseMatrix)) {                    
                message("Getting Cached Inverse Matrix")
                return(inverseMatrix)                        
        }
        MatrixData <- x$getMatrix()                 
        inverseMatrix <- solve(MatrixData, ...)  
        x$setInverse(inverseMatrix)                     
        return(inverseMatrix)                        
        ## Return a matrix that is the inverse of 'x'
}


