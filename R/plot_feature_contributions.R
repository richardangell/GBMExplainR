#' Plot feature contributions
#' 
#' Plot a simple barchart of feature contributions for a given prediction.
#' 
#' @param feature_contributions feature contributions output from 
#' \code{decompose_gbm_prediction}, note this must be run with 
#' \code{aggregate_contributions = TRUE}.
#' 
#' @return function does not return anything but plots barchart.
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
#' preds_decomp <- decompose_gbm_prediction(gbm1, data[1, ])
#' 
#' plot_feature_contributions(preds_decomp)
#' 
#' @export
plot_feature_contributions <- function(feature_contributions, ...) {
  
  if (sum(feature_contributions$variable == "Bias") != 1) {
    
    stop("feature_contributions should be the output from ",
         "decompose_gbm_prediction run with aggregate_contributions = TRUE")
    
  }

  feature_contributions <- 
    feature_contributions[order(feature_contributions$contribution), ]
  
  barplot(height = feature_contributions$contribution,
          names.arg = paste0(feature_contributions$variable,
                             "\n",
                             " (",
                             feature_contributions$variable_value,
                             ")"),
          main = "Feature Contributions",
          ...)
  
}
