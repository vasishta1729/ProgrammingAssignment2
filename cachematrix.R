## The cached inverse matrix (m) is initially set to a NULL value
## Saves the matrix to a variable x
## Returns a list with 1.setting the value of the matrix
## 2. getting the matrix
## 3. saving and setting the inverse matrix
## 4. getting the cached inverse matrix
## This function does not calculate the inverse matrix. It just saves it and only ## returns when called. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL ## setting the cached inverse matrix to NULL initially
        set <- function(y){
                x <<- y ## setting the variable x within the environment
                m <<- NULL ## resetting the cached inverse matrix to NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve ## setting the function to set the value of m to the cached inverse matrix 
        getinverse <- function() m ## getting the cached inverse matrix to the variable getinverse
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}



## Checks if the inverse is already cached and returns the cached inverse value
## If there is no inverse cached, the inverse is calculated for the matrix stored in x and saves it into m using the 'solve' function and returns the cached inverse matrix

cacheSolve <- function(x, ...) {
        m <- x$getinverse() #setting the data from getinverse variable to m
        if(!is.null(m)) { 
                message("getting cached data")
                return(m)
        } #checking if there is a cached matrix
        data <- x$get() #assigning the given matrix to data in case there is no cached matrix
        m <- solve(data, ...) #calculating the inverse value and saving it to m
        x$setinverse(m) 
        m #returning the cached inverse matrix
}

matrixA <- matrix(data = c(2,5,6,8,9,11,7,5,13), nrow=3, ncol= 3)
matrixB <- makeCacheMatrix(matrixA)
cacheSolve(matrixB)