# rm(list=ls()) # For cleaning the environment of R

# This function is responsible for caching the values
makeCacheMatrix<-function(cached_matrix=matrix()){
  
  # set a variable to cache the inverse of a matrix
  inversed_matrix<-NULL
  
  # function to set the value of the matrix we want to do the inverse
  set<-function(new_matrix){
    # cache the current value into `cached_matrix`
    cached_matrix<<-new_matrix 
    # initialize the variable again to store the cached value of inversed matrix to `inversed_matrix`
    inversed_matrix<<-NULL
  }
  
  # function to get the current matrix we assigned
  get<-function() cached_matrix
  
  # function to cache the value of the inversed matrix 
  inverseTheMatrix<-function(inversed_matrix_value)inversed_matrix<<-inversed_matrix_value
  
  # function to get the inversed value of our matrix
  getTheInversedMatrix<-function()inversed_matrix
  
  # returns the matrix of all the functions 
  list(set=set,
       get=get,
       inverseTheMatrix=inverseTheMatrix,
       getTheInversedMatrix=getTheInversedMatrix)
}

# This function takes the return value of the matrix
cacheSolve<-function(cached_matrix,...){
  
  # find the inversed data
  inversed_matrix<-cached_matrix$getTheInversedMatrix()
  
  # if the inversed data exists then print the inversed data
  if(!is.null(inversed_matrix)){
    message("getting the cached data")
    return (inversed_matrix)
  }
  
  # if the inversed data is not found get the current matrix
  my_matrix<-cached_matrix$get()
  
  # calculate the inverse of a matrix
  inversed_matrix_value<-solve(my_matrix,...)
  
  # store the inverse of the matrix
  cached_matrix$inverseTheMatrix(inversed_matrix_value)
  
  # return the inversed value of the matrix
  inversed_matrix_value
}

# Testing the functions
# test_matrix<-matrix(1:4,nrow=2,ncol=2)
# my_matrix<-makeCacheMatrix(cached_matrix)
# my_matrix$set(test_matrix)
# cacheSolve(my_matrix)
