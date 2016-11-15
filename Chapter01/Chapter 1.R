######################################################
#######   1.5 Basic Data types in R  #################  
######################################################

############  1.5.1 Operations in R  #################
V <- c(1,2,3,4,5,6)  ## Assigning a vector V
V
V+10 ## Adding each element in vector V with 10
V-1  ## Subtracting each element in vector V with 1
sqrt(V)  ## Performing square root operation on each element in vector V
V1 <- log(V)  ## Storing log transformed of vector V as V1
V1

######################################################
#######   1.6 Control structures in R  ###############  
######################################################

############  1.6.1 If. condition  #################
x <- 10
if (x < 5) print(x)
if (x > 5) print(x)

############  1.6.2 If.Else. condition  #################
x=10
if (x %% 2)
  {
     print(paste0(x, " :odd number"))
   } else {
     print(paste0(x, " :even number"))
  }

############  1.6.3 IfElse. function  #################
x <- 1:6
ifelse(x %% 2, paste0(x, " :odd number"), paste0(x, " :even number"))

############  1.6.4 For() loop  #################
x <- c("John", "Mary", "Paul", "Victoria")
for (i in seq(x)) 
  {
 print(x[i])
  }

############  1.6.5 Nested for( ) loop  #################
mat <- matrix(1:9, ncol = 3)
sum <- 0
for (i in seq(nrow(mat))) 
  {
   for (j in seq(ncol(mat))) 
     {
       sum <- sum + mat[i, j]
       print(sum)
     }
  }

############  1.6.6 While loop  #################
i <- 1
while (i < 10) 
   {
     print(i)
     i <- i + 1
   }

############  1.6.7 Special statements in Loops  #################
############  1.6.7.1 Break statement  #################
for (i in 1:30) 
   {
     if (i < 8) 
     {
       print(paste0("Current value is ",i))
     } else {
       print(paste0("Current value is ",i," and the loop breaks"))
       break
     }
   }

############  1.6.7.2 Next Statement  #################
for (i in 1:10) 
   {
     if (i %% 2) 
     {
       print(paste0(i, " is an odd number."))
     } else {
       next
     }
   }

############  1.6.7.2 Repeat loop  #################
i <- 1
repeat 
   {
     cube <- i ** 3
     i <- i + 1
     if (cube < 729) 
     {
       print(paste0(cube, " is less than 729. Let's remain in the loop."))
     } else {
       
       print(paste0(cube, " is greater than 729. Let's exit the loop."))
       break  
     }
   }

######################################################
######   1.7 First class functions in R  #############  
######################################################

############  Vectorised functions: Approach 1  #################
V_in <- 1:100000           ## Input Vector
V_out <- c()               ## Output Vector
for(i in V_in)     		     ## For loop on Input vector
     {
     V_out <- c(V_out,i^2) ## Storing on Output vector
     }

############  Vectorised functions: Approach 2  #################
V_in <- 1:100000               ## Input Vector
V_out <- V_in^2                ## Output Vector

############  Vectorised functions: Approach 3  #################
V_in <- 1:100000                      ## Input Vector
V_out <- sapply(V_in,function(x) x^2) ## Output Vector






