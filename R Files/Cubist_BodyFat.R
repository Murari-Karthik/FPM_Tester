# Install and load necessary packages (if not already installed)
if (!require("Cubist")) install.packages("Cubist")

# Load the bodyfat dataset from the URL
URL <- "https://raw.githubusercontent.com/aviralb13/git-codes/main/datas/bodyfat.csv"
bodyfat <- read.csv(URL)

# Specify the target variable name
target_variable <- "BodyFat"

# Split the data into features (X) and target variable (y)
X <- subset(bodyfat, select = -get(target_variable))
y <- bodyfat[[target_variable]]

# Perform feature scaling by subtracting mean and dividing by standard deviation
X_scaled <- scale(X)

# Split the data into training and testing sets
set.seed(42)  # Set seed for reproducibility
index <- sample(1:nrow(X), 0.7 * nrow(X))
X_train <- X_scaled[index, ]
X_test <- X_scaled[-index, ]
y_train <- y[index]
y_test <- y[-index]

# Load the Cubist library
library(Cubist)

# Train Cubist model
data_train <- data.frame(cbind(y_train, X_train))
cubist_model <- cubist(X_train, y_train)

# Make predictions on the test set
cubist_predictions <- predict(cubist_model, data.frame(X_test))

# Calculate MAE
mae <- mean(abs(cubist_predictions - y_test))

# Calculate PCC
pcc <- cor(cubist_predictions, y_test)

# Display results
cat("MAE:", mae, "\n")
cat("PCC:", pcc, "\n")
