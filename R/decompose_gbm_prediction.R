#' Decompose gbm prediction into feature contributions + bais
#' 
#' For a single observation decompose the prediction for a gbm into feature 
#' contributions + bais. Within a single tree, the contribution for a given 
#' node is calculated by subtracting the prediction for the current node from 
#' the prediction of the next node the observation would visit in the tree. 
#' The predicted value for the first node in the tree is combined into the 
#' bais term (which also includes the intercept or \code{initF} from the model).
#' Node contributions are summed by the split variable for the node, across all 
#' trees in the model, giving the observation's prediction represented as
#' bais + contribution for each feature used in the model.
#' 
#' @param gbm \code{gbm.object} to predict with
#' @param prediction_row single row \code{data.frame} to predict and the 
#'   decompose into feature contributions
#'   
#' @details Based on treeinterpreter Python package for random forests; 
#'   \url{https://github.com/andosa/treeinterpreter}.  
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
#' decompose_gbm_prediction(gbm1, data[1, ])
#' 
#' @export
decompose_gbm_prediction <- function(gbm, prediction_row, verbose = FALSE) {
  
  #-----------------------------------------------------------------------------#
  # Function | decompose_gbm_prediction
  #-----------------------------------------------------------------------------#
  # Layout   | Section 0. Input checking
  #          | Section 1. Input checking
  #          | Section 2. Determine feature contributions
  #          | Section 3. Return feature contributions
  #-----------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #-----------------------------------------------------------------------------#
  
  if (!is(gbm, "gbm")) {
    
    stop("gbm should be class gbm.")
    
  }
  
  if (!is(prediction_row, "data.frame")) {
    
    stop("prediction_row should be a data.frame")
    
  }
  
  if (nrow(prediction_row) != 1) {
    
    stop("prediction_row should be a single row data.frame")
    
  }
  
  supplied_columns <- colnames(prediction_row)
  
  if (any(!gbm$var.names %in% supplied_columns)) {
    
    stop("the following columns required by the model are not present; ",
         paste(gbm$var.names[which(!gbm$var.names %in% supplied_columns)]))
    
  }
  
  if (any(gbm$var.type > 0)) {
    
    for (i in which(gbm$var.type > 0)) {

      cat_value <- as.character(prediction_row[[gbm$var.names[i]]])
      
      if (!cat_value %in% gbm$var.levels[i][[1]]) {
        
        stop(paste(sQuote(cat_value), 
                   "is not an expected level for",
                   gbm$var.names[i]))  
        
      }
      
    }
    
  }
  
  #-----------------------------------------------------------------------------#
  # Section 1. Input checking ----
  #-----------------------------------------------------------------------------#
  
  explanatory_vars <- gbm$var.names
  
  model_intercept <- gbm$initF
  
  # loop through all trees in the model and get the decision route through 
  # each tree for the prediction row
  tree_routes <- lapply(1:gbm$n.trees,
                        function(x) {
                          
                          pretty_tree <- pretty.gbm.tree(gbm, i.tree = x)
                          
                          tree_route <- get_decision_path(pretty_tree = pretty_tree,
                                                          model = gbm, 
                                                          pred_row = prediction_row,
                                                          verbose = verbose)
                          
                          return(tree_route)
                          
                        })

  #-----------------------------------------------------------------------------#
  # Section 2. Determine feature contributions ----
  #-----------------------------------------------------------------------------#
  
  # append routes through all trees into one data.frame
  tree_routes_all <- do.call(rbind, tree_routes)
  
  levels(tree_routes_all$variable) <- c(levels(tree_routes_all$variable),
                                        "Bais")
  
  tree_routes_all$variable[tree_routes_all$direction == "TerminalNode"] <- "Bais"
  
  contributions <- aggregate(x = tree_routes_all$contrib,
                             by = list(tree_routes_all$variable),
                             FUN = sum)
  
  colnames(contributions) <- c("variable", "contribution")
  
  # add on the intercept to the bais term
  contributions[contributions$variable == "Bais", "contribution"] <- 
    contributions[contributions$variable == "Bais", "contribution"] + gbm$initF
  
  #-----------------------------------------------------------------------------#
  # Section 3. Return feature contributions ----
  #-----------------------------------------------------------------------------#
  
  return(contributions)
  
}


