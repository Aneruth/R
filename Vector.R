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