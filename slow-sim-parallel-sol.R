library(foreach)


#Frage: wo kommt dieser Code schnipsel hin? 
library(doParallel)
#library(parallel)

cluster <- makeCluster(3)
registerDoParallel(cluster)


# function to simulate reps different datasets and estimate the coefficeints of
# a linear model of y = data* true_coef + error, where the error is t(df)
# distributed 

# inputs : reps: num of repitions, seed: seed for drawing random numbers, data:
# data where the random error will be added, true_coef: vector of true
# coefficients, df: degrees of freedom for t distributet error

# output: 


# function to simulate reps different datasets and estimate the coefficeints of
# a linear model of y = data* true_coef + error, where the error is t(df)
# distributed

# inputs : reps: num of repitions, seed: seed for drawing random numbers, data:
# data where the random error will be added, true_coef: vector of true
# coefficients, df: degrees of freedom for t distributet error

# output:
simulate <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  assert_count(reps)
  assert_int(seed)
  assert_data_frame(data)
  assert_numeric(true_coef, len = ncol(data) + 1)
  assert_count(df)
  
  
  
  set.seed(seed)
  expected <- calculate_expected(data, true_coef)

  coefs <- foreach(rep = seq_len(reps), .combine = 'cbind') %dopar% {
    simulate_once(expected, df, data)
  }
  return(structure(coefs, seed = seed))
}


simulate <- function(reps, seed, data, true_coef = 0:ncol(data), df = 4) {
  set.seed(seed)
  expected <- calculate_expected(data, true_coef)
  
  coefs <- foreach(rep = seq_len(reps), .combine = 'cbind') %dopar% {
    simulate_once(expected, df, data)
  }
  
  
  return(structure(coefs, seed = seed))
}


# function to calculate the expected values for y

# inputs: data and true coefficients

# output: expected data
calculate_expected <- function(data, true_coef) {
  design <- model.matrix(~., data = data)
  expected <- design %*% true_coef
  expected
}


# function to extimate the coeficcients of a linear model.

# inputs: data, expected data y = X*beta, df degrees of freedom of t distribution error

# output: estimated coefficients of linear model
simulate_once <- function(expected, df, data) {
  data <- simulate_response(expected, df, data)
  estimate_coef(data)
}


# function to simulate response of a model y = X*beta+ e, where e ~t(4)

# inputs: data, expected data y = X*beta, df degrees of freedom of t distribution error

# output: data with random t error y =X*beta+epsilon
simulate_response <- function(expected, df, data) {
  # only rt is a random process
  data[["y"]] <- expected + rt(nrow(expected), df = df)
  data
}


# function to estimate coefficients of a linear model

# input: data

# output: coefficients
estimate_coef <- function(data) {
  model <- lm(y ~ ., data = data)
  unname(coef(model))
}

