library(deepnet)
library(caret) # Package to perform one hot encoding

# Load the dataset
test <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Test_set_values.csv')
train <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_values.csv')
labels <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_labels.csv')

train <- data.frame(cbind(train,as.factor(labels$status_group))) # Considering the status as factor column
names(train)[41] <- 'status_group'

test <- data.frame(cbind(test,as.factor(labels$status_group))) # Considering the status as factor column
names(test)[41] <- 'status_group'

# To get the unique values of recorded_by
unique(train['recorded_by'])
# Since we can see that 'recorded_by' column has no unique elements present so we can drop.
train <- train[ , !names(train) %in% c('recorded_by')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('recorded_by')]

# Checking our dataset dimension after dropping a column 
dim(train) 

# Checking our second column amount_tsh
str(train['amount_tsh'])
table(train$amount_tsh)
# From the below table we can say that it consist off more zeros so we can drop it
train <- train[ , !names(train) %in% c('amount_tsh')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('amount_tsh')]

# dim(train) # Checking our dataset dimension

# In public meeting we are removing that column 
train <- train[ , !names(train) %in% c('public_meeting')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('public_meeting')]

dim(train)

# Checking permit column
table(train$permit)
# From below table we can see that there is a empty string and we can't take the mean values as it is boolean value
# So drop that column
train <- train[ , !names(train) %in% c('permit')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('permit')]

dim(train)

# Grouping extraction type,extraction type group and extraction type class
View(data.frame(train$extraction_type,train$extraction_type_group,train$extraction_type_class))
# We can say that three column have a common value and hence we can remove one column
train <- train[ , !names(train) %in% c('extraction_type_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('extraction_type_group')]

######################################## Get back to it later
# Grouping management and management_group
View(data.frame(train$management,train$management_group))
# We can't drop anything from above mentioned columns as they posses different values

# Compare scheme name and scheme management
View(data.frame(train$scheme_name,train$scheme_management))
table(train$scheme_name) # Checking the table of values for scheme_name
table(train$scheme_management) # Checking the table of values for scheme_management
# Since scheme_name consist of too many categorical values we can eliminate it
train <- train[ , !names(train) %in% c('scheme_name')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('scheme_name')]

dim(train)

# Comparing the payment and payment_type
View(train[c('payment','payment_type')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('payment')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('payment')]

dim(train)
dim(test)

# Comparing the water_quality and quality_group
View(train[c('water_quality','quality_group')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('quality_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('quality_group')]

dim(train)
dim(test)

# Comparing the quantity and quantity_group
View(train[c('quantity','quantity_group')])
# Since we can infer that they both have a close relation so we can eliminate either of the column
train <- train[ , !names(train) %in% c('quantity_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('quantity_group')]

# dim(train)

# Comparing the source_type, source and source_class
View(train[c('source_type','source','source_class')])
# By comparing we can say that source_type and source are same so we can drop either of it but source_class defines type of the water source
train <- train[ , !names(train) %in% c('source_type')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('source_type')]

# dim(train)

# Comparing the source_type, source and source_class
View(train[c('waterpoint_type','waterpoint_type_group')])
# By comparing we can say that source_type and source are same so we can drop either of it but source_class defines type of the water source
train <- train[ , !names(train) %in% c('waterpoint_type_group')]

# Doing the same for test dataset
test <- test[ , !names(test) %in% c('waterpoint_type_group')]

# dim(train)

# drop columns
train <- train[ , !names(train) %in% c('id','wpt_name','num_private','lga','ward','amount_tsh','subvillage','region','funder','installer')]

# Doing the above for test data
test <- test[ , !names(test) %in% c('id','wpt_name','num_private','lga','ward','amount_tsh','subvillage','region','funder','installer')]

dim(train)
dim(test)

# Converting the date to days
train$date_recorded = as.numeric(as.Date(Sys.Date()) - as.Date(c(train$date_recorded)))
test$date_recorded = as.numeric(as.Date(Sys.Date()) - as.Date(c(test$date_recorded)))
View(train)
View(test)

# Replacing all 0 with NA

# After analyzing we can say that 0's are present in two columns that is population and construction year so we consider their mean value
replace_zeros <- function(x){
  # converting that particular columns 0 values to NA and then considering the mean
  x$population[x$population == 0] <- NA
  x$construction_year[x$construction_year == 0] <- NA
  
  # Replacing the NA values to its respective mean
  x$population <- ifelse(is.na(x$population), round(mean(x$population, na.rm=TRUE)), x$population)
  x$construction_year <- ifelse(is.na(x$construction_year),round( mean(x$construction_year, na.rm=TRUE)), x$construction_year)
  x$gps_height <- ifelse(is.na(x$gps_height),round( mean(x$gps_height, na.rm=TRUE)), x$gps_height)
  
  return(x) # returning the dataset
}

df_train <- replace_zeros(train)
df_test <- replace_zeros(test)
View(df_train)
View(df_test)

# Converting the categorical value of to top 4 categorical values 
clean_dataset <- function(x){
  x$basin <- ifelse(x$basin == 'Lake Victoria','lake victoria',x$basin)
  x$basin <- ifelse(x$basin == 'Pangani','pangani',x$basin)
  x$basin <- ifelse(x$basin == 'Rufiji','rufiji',x$basin)
  x$basin <- ifelse(x$basin == 'Internal','internal',x$basin)
  x$basin <- ifelse(x$basin != 'lake victoria' & x$basin != 'pangani' & x$basin != 'rufiji' & x$basin != 'internal' ,'other_basin',x$basin)
  
  # Considering the scheme_management column
  x$scheme_management <- ifelse(x$scheme_management == 'VWC','vwc',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'WUG','wug',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'Water authority','water_auth',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management == 'Water Board','water_board',x$scheme_management)
  x$scheme_management <- ifelse(x$scheme_management != 'vwc' & x$scheme_management != 'wug' & x$scheme_management != 'water_auth' & x$scheme_management != 'water_board' ,'other_schmgt',x$scheme_management)
  
  # Considering the extraction_type column
  x$extraction_type <- ifelse(x$extraction_type != 'gravity','other_exttype',x$extraction_type)
  
  # Considering the extraction_type_class column
  x$extraction_type_class <- ifelse(x$extraction_type_class != 'gravity' & x$extraction_type_class != 'handpump' ,'other_extclass',x$extraction_type_class)
  
  # Considering the management column
  x$management <- ifelse(x$management != 'vwc' & x$management != 'wug' & x$management != 'wua','other_mgt',x$management)
  
  # Considering the management_group column
  x$management_group <- ifelse(x$management_group != 'user-group','other_mgtgroup',x$management_group)
  
  # Considering the payment_type column
  x$payment_type <- ifelse(x$payment_type != 'never pay','other_payemt',x$payment_type)
  
  # Considering the water_quality column
  x$water_quality <- ifelse(x$water_quality != 'soft' ,'other_waterqual',x$water_quality)
  
  # Considering the quantity column
  x$quantity <- ifelse(x$quantity != 'insufficient' & x$quantity != 'enough','other_quantity',x$quantity)
  
  # Considering the source column
  x$source <- ifelse(x$source != 'shallow well' & x$source != 'spring' & x$source != 'machine dbh' & x$source != 'river','other_source',x$source)
  
  # Considering the source_class column and converting unknown to other_sourceClass
  x$source_class <- ifelse(x$source_class == 'unknown','other_sourceClass',x$source_class)
  
  # Considering the waterpoint_type column
  x$waterpoint_type <- ifelse(x$waterpoint_type != 'communal standpipe' & x$waterpoint_type != 'hand pump','other_watertype',x$waterpoint_type)
  return(x) # Returns the dataset
}

labels_rename <- function(x){
  x$status_group <- ifelse(x$status_group == "functional",as.numeric(0),x$status_group)
  x$status_group <- ifelse(x$status_group == "non functional",as.numeric(1),x$status_group)
  x$status_group <- ifelse(x$status_group == "functional needs repair",as.numeric(2),x$status_group)
  return(x)
}

cleaned_train <- clean_dataset(labels_rename(df_train))
cleaned_test <- clean_dataset(labels_rename(df_test))

View(cleaned_train)
View(cleaned_test)

# Performing one hot encoding
train_dmy <- caret::dummyVars(' ~ .',data = cleaned_train,fullRank = T)
train_1h <- data.frame(predict(train_dmy, newdata = cleaned_train))

test_dmy <- caret::dummyVars(' ~ .',data = cleaned_test,fullRank = T)
test_1h <- data.frame(predict(test_dmy, newdata = cleaned_test))

View(train_1h)
View(test_1h)

# Library to be used
library(keras)
library(tfestimators) # Modeling helper package addtl grid search & model run interface
require(keras)
require(tensorflow)


nn <- neuralnet(status_group ~., data = train_1h,hidden = 2 ,linear.output = FALSE)
nn$weights
nn$result.matrix
plot(nn)

nn$covariate # Inputs

# To check the prediction of the model
nn$net.result[[1]]
nn1 = ifelse(nn$net.result[[1]] > 0.5,1,0)
nn1
erro = mean(train_1h$status_group != nn1)
erro
