####################         CHAPTER 2: ALGORITHM ANALYSIS            #############################

######################################################
#######   2.2 Memory management in R  ################  
######################################################

library(pryr)
## Examples of object_size()

object_size(1)    ## Memory allocated for a single numeric vector
object_size("R")  ## Memory allocated for a single character vector
object_size(TRUE) ## Memory allocated for a single logical vector
object_size(1i)   ## Memory allocated for a single complex vector

object_size(integer())
object_size(character())
object_size(logical())
object_size(complex())
object_size(vector())
object_size(list())
object_size(matrix())
object_size(array())
object_size(data.frame())
object_size(data.table())

## Code to show how memory management happens in numeric, character, complex and logical vector
vec_length <- 0:60
num_vec_size <- sapply(vec_length, function(x) object_size(seq(x)))
char_vec_size <- sapply(vec_length, function(x) object_size(rep("a",x)))
log_vec_size <- sapply(vec_length, function(x) object_size(rep(TRUE,x)))
comp_vec_size <- sapply(vec_length, function(x) object_size(rep("2i",x)))

par(mfrow=c(2,2))
plot(num_vec_size ~ vec_length, xlab = "Numeric seq vector", ylab = "Memory allocated (in bytes)", 
     type = "n")
abline(h = (c(0,8,16,32,48,64,128)+40), col = "grey")
lines(num_vec_size, type = "S")
plot(char_vec_size ~ vec_length, xlab = "Character seq vector", ylab = "Memory allocated (in bytes)", 
     type = "n")
abline(h = (c(0,56,64,80,96,112,176)+40), col = "grey")
lines(char_vec_size, type = "S")
plot(log_vec_size ~ vec_length, xlab = "Logical seq vector", ylab = "Memory allocated (in bytes)", 
     type = "n")
abline(h = (c(0,8,16,32,48,64,128)+40), col = "grey")
lines(log_vec_size, type = "S")
plot(comp_vec_size ~ vec_length, xlab = "Complex seq vector", ylab = "Memory allocated (in bytes)", 
     type = "n")
abline(h = (c(0,56,64,80,96,112,176)+40), col = "grey")
lines(comp_vec_size, type = "S")

######################################################
#######   2.3 System Run-Time in R  ##################  
######################################################

## Aggregate function
Agg <- aggregate(mpg~carb,data=mtcars,mean)

## Plyr package
library(plyr)
DDply <- ddply( mtcars, .(carb),function(x) mean(x$mpg))

## Data.table format
library(data.table)
mtcars_tb <- data.table(mtcars)
setkey(mtcars_tb,carb)
mtcars_tb[,mean(mpg),by=carb]

## Summarize function
library(dplyr)
summarize(group_by(mtcars, carb), mean(mpg))

library(microbenchmark)
MB_res <- microbenchmark(
  Aggregate_func=aggregate(mpg~carb,data=mtcars,mean),
  Ddply_func=ddply( mtcars, .(carb),function(x) mean(x$mpg)),
  Data_table_func = mtcars_tb[,mean(mpg),by=carb],
  Group_by_func = summarize(group_by(mtcars, carb), mean(mpg)),
  times=1000
)

library(ggplot2)
autoplot(MB_res)

######################################################
#####   2.7 Computation evaluation of a Program  #####
######################################################

############  Component 1: Assignment operator  #####

int_Vector <- 0:60

############  Component 2: Simple loop  #############

a <- 0
for(i in 1:n)
  a <- a + i

############  Component 3: Complex loop  ############

a <- 1
i <- 1
b <- list()
while(i<=n )
{
  a <- a + i
  i<- i+1
}
for(j in 1:i)
  for(k in 1:i)
  {
    b[[j]] <- a+j*k
  }

############  Component 4: Loops with conditional statements  ############

a <- 1
for(i in 1:n)
{
  if(i <= n/2)
  {
    for(j in 1:i)
      a <- a+i
  }else{
    a <- a*i
  }
}

############  Component 5: Recursive statements  ############

fact_n <- 1
for(i in 2:n)
{
  fact_n <- fact_n * i
}






