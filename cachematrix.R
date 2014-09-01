## creates a matrix that can be cached and inverted, returns a list to set matrix,
## get matrix, set inverse, and get inverse 
makeCacheMatrix <- function(x = matrix()) {
        
        # set inverse matrix as NULL
        inv <- NULL
        
        # cache the matrix as x and the inverse as NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # function to retrieve matrix
        get <- function() x
        
        # function to set the inverse to the cached variable inv
        setinverse <- function(inverse) inv <<- inverse
        
        # function to retrieve inverse matrix
        getinverse <- function() inv
        
        # returns list of functions
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# function that returns the inverse matrix of x, if this matrix has been stored in cache
# then the cached matrix is returned, otherwise a inverse matrix is calculated using solve()
cacheSolve <- function(x, ...) {
        
        #assign inv using getinverse from previous function
        inv <- x$getinverse()
        
        #check if the value assigned to inv is not null
        if(!is.null(inv)) {
                
                #if the value is not null then display message and return the value cached to inv
                message("getting cached data")
                return(inv)
        }
        
        # assign the matrix to data using get from the previous function
        data <- x$get()
        
        # find the inverse matrix for data using solve()
        inv <- solve(data)
        
        # use setinverse to cache the newly calculated inverse matrix
        x$setinverse(inv)
        
        # return the inverse matrix
        inv
}