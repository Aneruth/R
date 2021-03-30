# Addition of two Numbers
A <- 12
B <- 14
C <- A + B 
# To print the value just call the object name itself
C

# To run two variables at one time highlight the variable
# and 'cmd + return'. 
var1 <- 2.4
var2 <- 12.43

# Division and Multiplication Method
resd <- var2 / var1
resm <- var2 * var1

# To print both the values 
resd
resm

# Using Inbuilt functions in R. 
# To find the square root of a particular variable
ans <- sqrt(A)

# Character Operations
# Concatenate two character
first_name <- "Aneruth"
last_name <- "Mohanasundaram"
full_name <- paste(first_name,last_name)
full_name

## Logical variables and Operations
# Can have two possible values, either T or F. 
# Note it has to be in Caps. 
# Operators : 1.< 
#             2.> 
#             3.==
#             4.!=
#             5.<=
#             6.>=
#             7.<=
#             8.!
#             9.|
#             10.&
#             11.isTRUE(x)
xc <- 3<4
xc

# Using a not operator
res <- !T
res

# Using a OR operator
res1 <- F
res | res1
# AND Operator
res & T
T & T 
