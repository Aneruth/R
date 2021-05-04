# Libraries importing


# To read our dataset
test <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Test_set_values.csv')
train <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_values.csv')
labels <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_labels.csv')

# To check the summary of our dataset.
test_summary <- summary(test)
train_summary <- summary(train)
label_summary <- summary(labels)

# To check the unique values
unique(labels[c(2)])

# To view our dataset.
View(test)
View(train)

# Visualizing our target column
as.data.frame(table(train$status_group))

###########################################################################################################################
############################################## Data Preprocessing #########################################################
###########################################################################################################################
# Fetching our dataset information
str(train)

#  To check missing values in our dataset 
count_amttsh <- table(train$amount_tsh)
count_schname <- table(train$scheme_name)
count_publicmeet <- table(train$public_meeting)
count_permit <- table(train$permit)

# Print those missing values
count_amttsh
count_schname
count_publicmeet
count_permit

count_val <- length(subset(train$amount_tsh,train$amount_tsh==0))
count_val

###########################################################################################################################
############################################## Column  Drop ###############################################################
###########################################################################################################################

# Dropping Source type column
drop <- c('amount_tsh','source_type','recorded_by','permit','public_meeting','scheme_name','waterpoint_type','quality_group','longitude','latitude')
train <- train[ , !(names(train) %in% drop)]

# Checking our dataset dimension
dim(train) # From 40 columns we can say that it got reduced to 33 columns.

# Testing
df <- train
df <- replace(df$construction_year, df$construction_year==0,mean(df$construction_year))
