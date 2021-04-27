# R Specific Method
x <- rnorm(8)
for(i in x){
  print(i)
}

# Conventional Method 
# Similar to python range function 
for (j in 1:5) { # Considers first five elements and prints it
  print(x[j])
}


# Compare Vectorized method and de-vectorized method 
N <- 100
a <- rnorm(N)
b <- rnorm(N)

# Vectorized Approach
c <- a * b

# De-vectorized Approach
# Loop throgh all the elements and multiply it 