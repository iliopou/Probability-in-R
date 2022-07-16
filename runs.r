#### Vasileios Iliopoulos 

#### This script addresses Question 2. 

#' title: "runs-report"
#' author: "Vasileios Iliopoulos"
#' date: "07/07/2022"

# Part (a)

N = 2000  

r = runif(N,0,1) # Generate 2000 random numbers from the uniform distribution.

x = matrix(NA,nrow = N,ncol = 1) # Column vector of 2000 rows.
for (i in 1:N)                   # the for loop saves Head or Tails to each cell of vector x.
{
  if(r[i] < 0.5 )     # 0.5 may be changed to any other number in (0, 1).
    { x[i] = "H" } else 
        { x[i] = "T" }
}

 
# Probability of Head 
P_H = length(which(x=="H"))/N
print(P_H)

# Probability of Tail 
P_T = length(which(x=="T"))/N
print(P_T)

# Part (b)

z <- c(x) 

result <- max(rle(z)$lengths[rle(z)$values == "H"]) # Computes the length of the longest run of heads

print(paste("The length of the longest run of heads is", result))

# Part (c)

# Do hypothesis test via simulation of a large number of fair coin tosses and count the proportion of longest run of heads being equal to 6.
# Null hypothesis (H_0): Probability of Head 1/2. Alternative hypothesis (H_1): Probability of Head not equal to 1/2.
# Significance level set to 5%. That is the probability of wrongly reject null (type 1 error).

n = 1000            # Number of tosses per each simulation.
sims = 20000 # Number of simulations.
count= 0      # Variable counting occurrences of 6 as the size of longest heads run at each simulation.


for (j in 1:sims)
{
  toss_j <-sample(c("H", "T") , size=n, replace=TRUE , prob=c(0.5, 0.5) )  # Note that at each iteration sample function returns a random number.
  
  y <- c(toss_j)
  
  result2 <- max(rle(y)$lengths[rle(y)$values == "H"]) # Computes the longest run of heads
  
  if (any(result2==6)) { count <- count + 1 } # At exit of the loop this variable counts the total number of longest runs of heads each with 6 elements.
}

print(count) 
pvalue <- count/sims # probability value based on simulation. This is the probability of getting longest run of heads 6, assuming the null hypothesis is correct.
print (pvalue)

if (pvalue < 0.05 ) {
  print("The result is statistically significant at a level of 5 percent; hence we do not accept the null hypothesis that the coin is fair.") 
  } else { print("We do not reject the null hypothesis.") }



