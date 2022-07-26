insertion_sort <- function(A) {if (length(A) > 1) {
  for (j in 2:length(A)) {
    key  <- A[j] 
    i <- j - 1 
    while (isTRUE(i > 0 && A[i] > key)) {             
      A[(i + 1)] <- A[i]
      i <- i - 1 
    }
    A[(i + 1)] <- key
  }
 }
 return(A)                               # In place sorting
}



