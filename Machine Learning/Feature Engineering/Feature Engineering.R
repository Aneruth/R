# Libraries importing


# To read our dataset
test <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Test_set_values.csv')
train <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_values.csv')
labels <- read.csv('/Users/aneruthmohanasundaram/Documents/GitHub/R-Basics/Machine Learning/Datasets/Training_set_labels.csv')

# To check the summary of our dataset.
test_summary <- summary(test)
train_summary <- summary(train)
label_summary <- summary(labels)

# To view our dataset.
View(test)

# Creating a dummy variable
# dummy1 <- data.frame(train)
# dummy2 <- data.frame(labels)
# View(dummy2)

# Merging our train values with train labels (Fo r testing we merge dummy2 with dummy1 based on ID)
train_df <- merge(train, labels, by.y = "id")
dim(train_df)
View(train_df)

# Test case
amna <- train_df[c(0:1), c(0:41)]
View(amna)