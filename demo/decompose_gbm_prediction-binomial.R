
# Section 0. Load packages ----

# required for dataset with poisson response
library(insuranceData)
library(gbm)
library(GbmExplainR)

# Section 1. Load data ----

data(dataCar)

# Section 2. Build gbm ----

set.seed(1)

expl_cols <- c("veh_value", 
               "veh_body", 
               "veh_age", 
               "gender", 
               "area", 
               "agecat", 
               "offset(log(exposure))")

car_gbm <- gbm(reformulate(expl_cols, "clm"),
               distribution = "bernoulli",
               data = dataCar,
               n.trees = 100,
               interaction.depth = 3,
               shrinkage = 0.01,
               bag.fraction = 0.5,
               train.fraction = 0.75)

# Section 3. Get model predictions ----

p_resp <- predict(car_gbm, 
                  dataCar,
                  type = "response")

p_link <- predict(car_gbm, 
                  dataCar,
                  type = "link")

head(p_link)
head(p_resp)

# Section 4. Decompose predictions ----

a <- decompose_gbm_prediction(gbm = car_gbm, 
                              prediction_row = dataCar[1, ], 
                              type = "link", 
                              verbose = FALSE, 
                              aggregate_contributions = TRUE,
                              n_trees = 100) 

sum(a$contribution)

p_link[1]

exp(p_link[1]) / (1 + exp(p_link[1]))

p_resp[1]

# results in error 
b <- decompose_gbm_prediction(gbm = car_gbm, 
                              prediction_row = dataCar[1, ], 
                              type = "response", 
                              verbose = FALSE, 
                              aggregate_contributions = TRUE,
                              n_trees = 100) 


