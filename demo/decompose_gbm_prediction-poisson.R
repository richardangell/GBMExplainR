
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

car_gbm <- gbm(reformulate(expl_cols, "numclaims"), 
               distribution = "poisson",
               data = dataCar,
               n.trees = 100,
               interaction.depth = 3,
               shrinkage = 0.01,
               bag.fraction = 0.5,
               train.fraction = 0.75)

# Section 3. Get model predictions ----

p_resp <- predict(car_gbm, dataCar, type = "response")

p_link <- predict(car_gbm, dataCar, type = "link")

head(p_resp)

head(p_link)

sum(p_resp[1:(0.75*nrow(dataCar))] * 
      dataCar[1:(0.75*nrow(dataCar)), "exposure"]) / 
  sum(dataCar[1:(0.75*nrow(dataCar)), "exposure"]) 

sum(dataCar[1:(0.75*nrow(dataCar)), "numclaims"]) / 
  sum(dataCar[1:(0.75*nrow(dataCar)), "exposure"]) 

# Section 4. Decompose predictions on link scale ----

a <- decompose_gbm_prediction(gbm = car_gbm, 
                              prediction_row = dataCar[1, ], 
                              type = "link", 
                              verbose = FALSE, 
                              aggregate_contributions = TRUE,
                              n_trees = 50) 

sum(a$contribution)

# check that sum of contributions for the first 50 trees equals the prediction
# on the log scale
predict(car_gbm, 
        dataCar[1 , ],
        type = "link",
        n.trees = 50)

a_not_aggregated <- decompose_gbm_prediction(gbm = car_gbm, 
                                             prediction_row = dataCar[1, ], 
                                             type = "link", 
                                             verbose = FALSE, 
                                             aggregate_contributions = FALSE,
                                             n_trees = 50) 

# note, model intercept not included in non aggregated view
car_gbm$initF + sum(a_not_aggregated$contrib)

# Section 5. Decompose predictions on response scale ----

b <- decompose_gbm_prediction(gbm = car_gbm, 
                              prediction_row = dataCar[2, ], 
                              type = "response", 
                              verbose = FALSE, 
                              aggregate_contributions = TRUE,
                              n_trees = 75) 

prod(b$contribution)

# for poisson gbm contributions on response scale multiply to give prediction
predict(car_gbm, 
        dataCar[2, ],
        type = "response",
        n.trees = 75)




