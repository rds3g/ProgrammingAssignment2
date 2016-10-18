
## my functions makeCacheMatrix() and cacheSolve() are very similar to
## the makeVector() and cachemean() functions provided.
## 
## instead of mean 'm', these functions calculate, store, and send back
## the desired matrix inverse of x; the inverse matrix is stored in 'iMatrix'
## 
## A new feature added to these functions is a provided by a new variable
## 'xIsDirty'.  In this context "Dirty" means that x has been set by the
## set() method and therefore makes "old" or "stale" the inverse stored
## in x.
##
## the new check on 'xIsDirty' is implemented to meet the requirement hinted in
## the instructions: 
##    "cacheSolve: This function computes the inverse of the special "matrix"
##     returned by makeCacheMatrix above. If the inverse has already been calculated
##    (and the matrix has not changed)"
##
## A check is made in cacheSolve() with this test:
##    if(xIsDirty==FALSE &   !is.null(iMatrix)){
## if this test passes, then the cached matrix is returned,
## otherwise the invsere is recalculated

## makeCacheMatrix()

## usage:  
## Step 1) Create an object to store the matrix and it's inverse
##         ##source ("cachematrix.R")
##         a <- matrix(c(1,2,3,4),2,2)
##         A <- makeCacheMatrix(a)
##
## Step 2) Get the inverse of a
##         aInv <- cacheSolve(A)

## update the matrix a to be inverted, get new inverse
##         aNew <- matrix(c(3,4,1,2),2,2)
##         A$set(aNew)
##         aNewInv <- cacheSolve(A)



makeCacheMatrix <- function(x = matrix()) {
  
    iMatrix <- NULL  # setup the initial pointer to the inverse Matrix
    xIsDirty <<- TRUE
    set <- function(y) {
      x <<- y
      iMatrix <<- NULL
      xIsDirty <<- TRUE    # this flag will be used to help determine if the 
                           # matrix 'x' has been either set for first time, or reset
                           # if reset, the iMatrix will again need to be recomputed
    }
    get <- function() {
      x
      }
    set_iMatrix <- function (new_iMatrix) {
      iMatrix <<- new_iMatrix
      xIsDirty <<- TRUE
    }
                           # ??? not sure about this yet
    get_iMatrix <- function () {
         iMatrix
    }
    isDirty <- function() {
         xIsDirty
    }
    list(set = set, get = get, 
         get_iMatrix = get_iMatrix,
         set_iMatrix = set_iMatrix,
         isDirty = isDirty)
      

}



## cacheSolve()
##
## with object A created by makeCacheMatrix(a), where a is an invertable matrix
## find inverse of a, denoted aInv by
## aInv <- cacheSolve(A)


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$get_iMatrix()
  if(xIsDirty==FALSE &   !is.null(iMatrix)){
    message("getting cached inversve matrix")
    return(iMatrix)
  }
  origMatrix <- x$get()
  iMatrix <- solve(origMatrix)
  x$set_iMatrix(iMatrix)
  xIsDirty <<- FALSE
  iMatrix
  }
