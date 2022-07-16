gcd <- function(x,y) 
{
 if (y==0) {return(x)}
  
  else
  {  
    while(y!=0) 
	{
    r<-x%%y;
    x<-y;
    y<-r;
    }
     return(x)
  }
}


num1 = as.integer(readline(prompt = "Enter first number: "))
num2 = as.integer(readline(prompt = "Enter second number: "))
print(paste("The gcd. of", num1,"and", num2,"is", gcd(num1, num2)))

