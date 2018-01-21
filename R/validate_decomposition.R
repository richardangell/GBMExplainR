#' Validate decomposition of gbm prediction
#' 
#' Compare decomposed prediction to the predicted value returned from 
#' \code{predict.gbm}. Predictions are compared up to and including every
#' tree in the model (i.e. comparisons are made up to and including 1st, 2nd,
#' ..., nth trees).
#' 
#' @param gbm \code{gbm.object} to predict with
#' @param prediction_row single row \code{data.frame} to predict and the 
#'   decompose into feature contributions
#' @param n_trees the number of trees to use in generating the prediction for
#'   the given row. Default \code{NULL} uses all trees in the model.
#' 
#' @examples
#' N <- 1000
#' X1 <- runif(N)
#' X2 <- 2*runif(N)
#' X3 <- ordered(sample(letters[1:4],N,replace=TRUE),levels=letters[4:1])
#' X4 <- factor(sample(letters[1:6],N,replace=TRUE))
#' X5 <- factor(sample(letters[1:3],N,replace=TRUE))
#' X6 <- 3*runif(N) 
#' mu <- c(-1,0,1,2)[as.numeric(X3)]
#' 
#' SNR <- 10 # signal-to-noise ratio
#' Y <- X1**1.5 + 2 * (X2**.5) + mu
#' sigma <- sqrt(var(Y)/SNR)
#' Y <- Y + rnorm(N,0,sigma)
#' 
#' # introduce some missing values
#' X1[sample(1:N,size=500)] <- NA
#' X4[sample(1:N,size=300)] <- NA
#' 
#' data <- data.frame(Y=Y,X1=X1,X2=X2,X3=X3,X4=X4,X5=X5,X6=X6)
#' 
#' # fit initial model
#' gbm1 <- gbm(Y~X1+X2+X3+X4+X5+X6,        
#'            data=data,                  
#'            var.monotone=c(0,0,0,0,0,0),
#'            distribution="gaussian",   
#'            n.trees=1000,     
#'            shrinkage=0.05,  
#'            interaction.depth=3,
#'            bag.fraction = 0.5,
#'            train.fraction = 0.5)
#' 
#' validate_decomposition(gbm1, data[1, ])
#' 
#' @export
validate_decomposition <- function(gbm, prediction_row, n_trees = NULL) {
  
  #----------------------------------------------------------------------------#
  # Function Layout
  # Section 0. Input checking
  # Section 1. Decompose prediction
  # Section 2. Get prediction from model
  # Section 3. Get difference between the two
  # Section 4. Retrun results
  #----------------------------------------------------------------------------#
  
  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#
  
  if (!is(gbm, "gbm")) {
    
    stop("gbm should be class gbm.")
    
  }
  
  if (!is(prediction_row, "data.frame")) {
    
    stop("prediction_row should be a data.frame")
    
  }
  
  if (nrow(prediction_row) != 1) {
    
    stop("prediction_row should be a single row data.frame")
    
  }
  
  if (gbm$distribution$name == "poisson") {
    
    warning("decomposed prediction will be validated on the link scale.")
    
  }
  
  # check n_trees is less than or equal to the number of trees in the model
  if (!is.null(n_trees)) {
    
    if (n_trees > gbm$n.trees) {
      
      stop("n_trees must be less than or equal to number of trees in the model")
      
    } else if (n_trees < 0)  {
      
      stop("n_trees must be greater than 0")
      
    }
    
  } else {
    
    n_trees <- gbm$n.trees
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 1. Decompose prediction ----
  #----------------------------------------------------------------------------#
  
  # do not aggregate to variable level
  contributions <- decompose_gbm_prediction(gbm = gbm, 
                                            prediction_row = prediction_row,
                                            type = "link",
                                            aggregate_contributions = FALSE,
                                            n_trees = n_trees)
  
  #----------------------------------------------------------------------------#
  # Section 2. Get predictions from model ----
  #----------------------------------------------------------------------------#
  
  # get predictions for each tree
  predictions <- predict.gbm(gbm, prediction_row, n.trees = 1:n_trees)
  
  predictions <- as.vector(predictions)
  
  #----------------------------------------------------------------------------#
  # Section 3. Check contribution for each tree ----
  #----------------------------------------------------------------------------#
  
  # recalculate the predicted value up to and including the ith tree
  decomposed_pred <- sapply(1:n_trees,
                            function(i) gbm$initF + 
                              sum(contributions[contributions$tree_no <= i, 
                                                "contrib"]))
  
  all_equal_check <- mapply(all.equal,
                            predictions,
                            decomposed_pred)
  
  return_df <- data.frame(model_predictions = predictions,
                          decomposed_predictions = decomposed_pred,
                          all_equal_check = all_equal_check)
  
  #----------------------------------------------------------------------------#
  # Section 4. Return results and print ----
  #----------------------------------------------------------------------------#
  
  if (is.character(return_df$all_equal_check)) {
    
    if (any(return_df$all_equal_check != "TRUE")) {
      
      non_equal_rows <- which(return_df$all_equal_check != "TRUE")
      
      for (i in non_equal_rows) {
        
        cat("tree no: ",
            i,
            "predict.gbm value: ",
            return_df[i, "model_predictions"],
            "decomposed prediction value: ",
            return_df[i, "decomposed_predictions"])
        
      }
      
    }
    
  }
  
  return(return_df)
   
}  
  

