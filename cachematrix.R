
## The following two functions, makeCacheMatrix, cacheSolve are designed to complete the following tasks:
## makeCacheMatrix - 
## 1. take a square numeric matrix as input parameter
## 2. create a variable in the closure to allow its later calculated inverse matrix to be cached.
## 3. create two setters and two getters to cache/retrieve the matrix and its inverse matrix
## 4. return a list that contains setting and getting functions.

## cacheSolve - 
## 1. take the special matrix created by calling makeCacheMatrix function as its input parameter
## 2. check if there exists an inverse matrix
## 3. if true returns the inverse matrix
## 4. if not then retrieve the matrix and call solve function to calculate its inverse matrix.
## 5. cache the resulting inverse matrix
## 6. return the resulting inverse matrix.

## Write a short comment describing this function

## this function is to create a special matrix object that can store its inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
      
      ## initialize inverse matrix variable 
      ## that will be used to cache its inverse matrix 
      invMtr <- NULL
      
      ## setter to replace existing matrix with a new one and remove existing inverse matrix
      set <- function (mtr) {
            invMtr <<- NULL
            x <<- mtr
      }
      
      ## getter to get stored matrix
      get <- function() x
      ## setter to replace existing inverse matrix with new one
      setInvMtr <- function(rvMtr) invMtr <<-rvMtr
      ## getter to get existing inverse matrix
      getInvMtr <- function() invMtr
      ## return a list for accessing the exectured closure and its values.
      list ( set=set,
             get=get,
             setInvMtr = setInvMtr,
             getInvMtr = getInvMtr)
}


## Write a short comment describing this function
## This function is to retrieve if cached the inverse matrix for a given matrix created by calling makeCacheMatrix function above.
## Otherwise it calcuates the inverse matrix for the given matrix, cache the resulting inverse matrix and then return it.
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invMtr <- x$getInvMtr()
      ## check if inverse matrix has been calculated.  
      ## if calculated already then returns the cached. 
      ## Otherwise it means that the matrix has been changed.
      if (!is.null(invMtr)){
            return (invMtr)
      }
      ## get existing matrix
      m <- x$get()
      ## calculate its inverse matrix
      invMtr <- solve(m)
      ## cache the calculated inverse matrix in the clourse created by makeCacheMatrix
      x$setInvMtr(invMtr)
      ## return calculated inverse matix
      invMtr
      
}
