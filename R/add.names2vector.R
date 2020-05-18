
add.names2vector<-function(x,cn){
  y <- as.vector(x)
  if (is.factor(x)) { y <- as.factor(y) }
  names(y) <- cn
  return(y)
}