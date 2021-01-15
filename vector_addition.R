# function created by Nik O'Brien #
# purpose is to perform and show vector addition/subtraction in 1,2 or 3 dimensions #

vector_add <- function(dimension = 2, ..., show_sum = FALSE){
  # libraries
  library(graphics)
  
  # store the incoming vectors in a list
  add_vectors <- list(...)
  
  # create null vector equal to dimension length
  result_vector <- rep(0, times = dimension)
  
  # perform the operation by adding each incoming vector to the null vector
  for(i in 1:length(add_vectors)){
    result_vector <- result_vector + add_vectors[[i]]
  }
  
  # this sets up the limits of the plot
  bigger_dim <- max(result_vector[1],result_vector[2])
  
  if(show_sum){
    # here we want to show each vector adding from head to tail
    plot(c(0,bigger_dim),c(0,bigger_dim))
    for(j in 1:length(add_vectors)){
      if(j==1){
        arrows(0,0,add_vectors[[j]][1],add_vectors[[j]][2])
      }else{
        arrows(add_vectors[[j-1]][1],add_vectors[[j-1]][2],
               add_vectors[[j-1]][1]+add_vectors[[j]][1],
               add_vectors[[j-1]][2]+add_vectors[[j]][2])
      }
    }
  }else{
    # here we only need to show the resulting vector
    plot(c(0,bigger_dim),c(0,bigger_dim))
  }
  
  # plot the resultant vector either way
  arrows(0,0,x1 = result_vector[1],y1 = result_vector[2], col = "blue")
  
  # return the result
  print(paste0("The resultant vector is: ", result_vector))
}
