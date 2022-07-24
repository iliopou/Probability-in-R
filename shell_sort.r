shell_sort <- function(A){
gaps <- c(rev((2^seq(0, floor(log(length(A)-1,2)), 1)))) ;       # Vector of increment sequence of powers of 2
  
 for (i in gaps) {
  
  for (el in i:length(A)) {                                     # Insertion sort i positions apart
      key <- A[el];
	    j <- el;
	   
  while (j > i && A[j-i] > key) {                               
	 A[j] <- A[j-i];
	 j <- j-i;
	 }
	 A[j] <- key 
	 }
  }                                                            # Last iteration is insertion sort 1 position apart

return (A)                                                     # In place sorting
}                                                              
  


