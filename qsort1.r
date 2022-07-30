qsort <- function (A, lo, hi) { if (lo < hi) {
pivot <- sample(A, 1); i <- lo; j <- hi;                                     # pivot is randomly chosen  
  
while(i <= j) {                                                              # Hoare's original partition scheme. At least n-1 comparisons and at most n+2 comparisons to partition n keys
  while (isTRUE(A[i] < pivot)) {i <- i + 1};
  while (isTRUE(A[j] > pivot)) {j <- j - 1};
   if (j < i) {break}                                                        # Ensure pointers not running off bounds 
      key <- A[i]; A[i] <- A[j]; A[j] <- key; i <- i + 1; j <- j - 1;
               }                                                         
A = qsort(A, lo, j);
A = qsort(A, i, hi)
       }

return(A)

  }


