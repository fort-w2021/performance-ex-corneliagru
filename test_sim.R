#source("slow-sim.R")
source("slow-sim-sol.R")
#source("slow-sim-parallel-sol.R")

set.seed <- 232323
observations <- 5000
covariates <- 10
testdata <- as.data.frame(
  matrix(rnorm(observations * covariates),
         nrow = observations
  ))

test <- simulate(reps = 100, seed = 20141028, data = testdata)

system.time(test <- simulate(reps = 100, seed = 20141028, data = testdata))

#debugonce(simulate)


slow <- function() {
  source("slow-sim.R")
  simulate(reps = 100, seed = 20141028, data = testdata)
}

better <- function() {
  source("slow-sim-sol.R")
  simulate(reps = 100, seed = 20141028, data = testdata)
}

parallel <- function() {
  source("slow-sim-parallel-sol.R")
  simulate(reps = 100, seed = 20141028, data = testdata)
}





bench::mark(slow = slow(), 
            better = better(),
            parallel = parallel(), check = FALSE)


