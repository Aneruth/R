# Creating a list
list_1 <- list(x = c(1:4),y = c(5:9))
list_2 <- c('A','B','C')
# To view full list
list_1

# To access only specific column from the data frame
list_1$x # X values
list_1$y # Y values

# To check its type
is.list(list_1)
is.list(list_2)

# Assigning the labels of our data frame
new_list <- list('Aneruth','Mohanasundaram',24)
# Creating the labels
names(new_list) <- c("Firt Name",'Last Name','Age')

# To access each columns 
new_list$`Firt Name`
new_list$`Last Name`
new_list$`Age`

# Using str function to display the list
str(new_list)

# Sub-setting the list
# Method 1 using index positions
list_1[[1]] # Accessing a particular object

# Method 2 using Name and Logical
new_list[['Last Name']] # Using Names
new_list[c(TRUE,FALSE,TRUE,FALSE)]

# Method 3 using "$" symbol
new_list$`Last Name`
