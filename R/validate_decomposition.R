#' Validate decomposition of gbm prediction
#' 
#' Compare the decomposed prediction to the predicted value returned from 
#' \code{predict.gbm}.
#' 
#' @param gbm \code{gbm.object} to predict with
#' @param prediction_row single row \code{data.frame} to predict and the 
#'   decompose into feature contributions
#' @param contributions optional \code{data.frame} output from 
#'   decompose_gbm_prediction. Default value is \code{NULL}, in which case 
#'   decompose_gbm_prediction will be called to decompose the predicted value
#'   for the input observation.
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
validate_decomposition <- function(gbm, prediction_row, contributions = NULL) {
  
  #-----------------------------------------------------------------------------#
  # Function | validate_decomposition
  #-----------------------------------------------------------------------------#
  # Layout   | Section 1. Decompose prediction
  #          | Section 2. Get prediction from model
  #          | Section 3. Get difference between the two
  #          | Section 4. Retrun results
  #-----------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------#
  # Section 1. Decompose prediction ----
  #-----------------------------------------------------------------------------#
  
  if (is.null(contributions)) {
    
    contributions <- decompose_gbm_prediction(gbm = gbm, 
                                              prediction_row = prediction_row)
    
  } else {
    
    if (!is(contributions, "data.frame")) {
      
      stop("contributions should be a data.frame (output from decompose_gbm_prediction)")
      
    }
    
    if (!all(colnames(contributions) == c("variable", "contribution"))) {
      
      stop("contributions should have columns; variable, contribution")
      
    } 
    
  }
  
  #-----------------------------------------------------------------------------#
  # Section 2. Get prediction from model ----
  #-----------------------------------------------------------------------------#

  prediction <- predict.gbm(gbm, prediction_row)
  
  #-----------------------------------------------------------------------------#
  # Section 3. Get difference between the two ----
  #-----------------------------------------------------------------------------#
  
  difference <- all.equal(prediction, sum(contributions$contribution))
  
  if (is.logical(difference)) {
    
    if (difference) {
      
      print("bais + feature contribtions = prediction")
      
    }
    
  } else {
    
    print(difference)
    
  }
  
  #-----------------------------------------------------------------------------#
  # Section 4. Retrun results ----
  #-----------------------------------------------------------------------------#
  
  return(list(prediction = prediction,
              contributions = contributions))
  
}