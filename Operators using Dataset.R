# Defining our dataset 
df.data <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/Implementation-of-breast-cancer-prediction-using-machine-learning-techniques./Datasets/BCCD.csv')

# to check the data we just call the variable name along with ".data" based on given condition
df.data

aneruth <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/Implementation-of-breast-cancer-prediction-using-machine-learning-techniques./Datasets/BCCD.csv')
ageColumn <- aneruth[aneruth$Age == 83]
ageColumn

# To fetch a column from dataset using vector method
cv <- aneruth[,c("Age")] # To fetch more column then we need to pass another column name as string. Similar to pandas datframe drop function.
cv

# To fetch a column based on index position
colmn <- aneruth[,c(2,3)]
colmn

# To fetch a column based on subset function using select argument
qwerty <- subset(aneruth,select = c('Age','BMI'))
qwerty