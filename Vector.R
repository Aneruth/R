vec <- c(2L,4L,15L,12L,345L)
vec1 <- c(2,4,15,12,345)
vec2 <- c('a','b','c')
# is.character(vec2)

# To print all odd numbers
odd <- seq(1,10,2)
odd

# To print even numbers 
eve <- seq(0,10,2)
eve
for(i in 1:10){
  if(i %% 2 ==0){
    print(i)
  }
}

print('End of this file')

# Mixed values in vector are not allowed. All values to be in similar to other values.

# A vector is a list which contains object of different class.
# An empty vector can be created using a vector function.



# Mixed Vector Creation 
mixedVecctor <- c(2,3,'Aneruth','Mohanasundaram')
class(mixedVecctor)

# Explicit coercion
# Objects can be converted from one class type to another by using as.* function

# Variable Assignment
x <- c(1:3)
y <- c(1:3)
z <- c(1:3)
class(x)

# Converting it to numeric
xNumeric <- as.numeric(x)
class(xNumeric)

# Converting to character
yChar <- as.character(y)
class(yChar)

# Converting to Boolean
zBoolean <- as.logical(z)
class(zBoolean)