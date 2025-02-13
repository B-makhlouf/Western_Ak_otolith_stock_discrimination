library(keras)
library(tensorflow)

# Generate synthetic data
num_samples <- 1000
time_steps <- 50
X <- array(rnorm(num_samples * time_steps), dim = c(num_samples, time_steps))
y <- rnorm(num_samples)

# Reshape data for 1D CNN
X <- array(X, dim = c(dim(X)[1], dim(X)[2], 1))

# Build the model
model <- keras_model_sequential() %>%
  layer_conv_1d(filters = 32, kernel_size = 3, activation = "relu", input_shape = c(time_steps, 1)) %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_conv_1d(filters = 64, kernel_size = 3, activation = "relu") %>%
  layer_max_pooling_1d(pool_size = 2) %>%
  layer_flatten() %>%
  layer_dense(units = 64, activation = "relu") %>%
  layer_dense(units = 1)  # Output layer for regression

# Compile the model
model %>% compile(
  optimizer = "adam",
  loss = "mse",
  metrics = c("mae")
)

# Train the model
history <- model %>% fit(
  X, y,
  epochs = 20,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model
model %>% evaluate(X, y)

# Make predictions
predictions <- model