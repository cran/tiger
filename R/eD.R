eD<-function(x,y){
   #Euclidean Distance
   #For multiple vectors in an array 
   #rows contain the k vectors
   #columns the n coordinates in the n-space
   #str(x) ==  matrix [1:k, 1:n]
   
   return(sqrt(sum((x-y)^2)))
}

