qsort <- function (A) { if (length(A) <= 1) return (A);           # base case 

pivot <- A[1];  small <- c(); big <- c(); keys <- A[-1];  # pivot is the leftmost element of A
  
for (i in keys) {ifelse(i < pivot, small <- c(small, i), big <- c(big, i))};  # each key is compared to pivot and accordingly is placed to small or big array - n-1 comparisons for partitioning n keys
                                                                              # small is an array for storing elements less than pivot
                                                                              # big is an array for storing elements greater than or equal to pivot 
small <- qsort(small);
big <- qsort(big);

return(c(small, pivot, big))
  
}




      
      