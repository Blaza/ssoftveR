# creating multinom model for predicting shape class
dat <- readRDS('training_data.RDS')
shapes_model <- multinom(class ~ . , dat, maxit = 1000)

