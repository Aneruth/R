# While Loop
# To put a logical expression
while(T){
  print("Hello World!")
}

# Counter Loop (To print values within a range using while loop)
counter <- 1
while(counter < 12){
  print(counter)
  counter <- counter + 1
}


# For Loop
# 1 to some value is given by [1:5] <-- Vector, array of numbers
for (i in 1:5) {
  print("Aneruth")
}

# To count even number present in a range.
count <- 0
for (i in 1:10){
  if (i %% 2 == 0){
    count = count + 1
  }
}
print(paste("Total even numbers present is",count))



## If statement and nested if statement

# To generate a random number
randomNumber <- rnorm(1)
if (randomNumber>1)
{
  print(TRUE)
}else{
  print(FALSE)
}

# to do nested if statement

## Method 1
if (randomNumber>1){
  print((paste(randomNumber,"is grater tahn 1")))
}else{
  
  if(randomNumber >= -1){
    print((paste(randomNumber,"is greater than -1")))
  }else{
    print(paste(randomNumber,"is lesser than -1"))
  }
}

## Method 2 Chain if else statement
x <- rnorm(1)
if (x>1){
  print(paste(x,"is greater than 1"))
}else if(x >= -1){
  print(paste(x,"is greather than or equal to -1"))
}else{
  print(paste(x,"is lesser than -1"))
}

