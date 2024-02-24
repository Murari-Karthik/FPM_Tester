# Install and load necessary packages (if not already installed)
if (!require("e1071")) install.packages("e1071")

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

# Load the e1071 library
library(e1071)

# Train SMOReg model
smo_model <- svm(y_train ~ ., data = data.frame(cbind(y_train, X_train)), kernel = "radial", cost = 1)

# Make predictions on the test set
smo_predictions <- predict(smo_model, data.frame(X_test))

# Calculate MAE
mae_smo <- mean(abs(smo_predictions - y_test))

# Calculate PCC
pcc_smo <- cor(smo_predictions, y_test)

# Display results for SMOReg
cat("SMOReg Model:\n")
cat("MAE:", mae_smo, "\n")
cat("PCC:", pcc_smo, "\n")
