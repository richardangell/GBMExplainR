#' Determine decision path down tree
#' 
#' This function calls \code{choose_split_r} which does the work to find the
#' decision path for the given observation. \code{get_decision_path} reformats
#' the output into a single \code{data.frame} and adds the contribution 
#' (\code{contrib} column) for each node.
#' 
#' @param prett_tree \code{data.frame} output from \code{pretty.gbm.tree}
#'   giving tree structure.
#' @param model \code{gbm.object} 
#' @param pred_row single \code{data.frame} row (containing explanatory columns) 
#'   to send down the tree to a terminal node
#' 
#' @return \code{data.frame} showing split decisions as well as contribution 
#'   to predicted value for each node visited by the given observation en route
#'   to a terminal node. Contains columns;
#'   \item{\code{variable}}{name of the splitting variable (NA for terminal 
#'     nodes)}
#'   \item{\code{type}}{type for splitting variable, if type > 0 then the 
#'     variable is categorical otherwise it is ordered or continuous (NA for
#'     terminal nodes)}
#'   \item{\code{direction}}{child node to travel down from current node}
#'   \item{\code{prediction}}{prediction at current node}
#'   \item{\code{contrib}}{current node contribution to the terminal node 
#'     prediction for the given observation in the given tree. Note, for 
#'     terminal nodes the contribution is the prediction for the first node in
#'     the tree. This is counted as part of the bais for the overall model.}
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
#'  get_decision_path(pretty_tree = pretty.gbm.tree(gbm1, 1), 
#'                    model = gbm1,
#'                    pred_row = data[1, ])          
#' 
#' @export
get_decision_path <- function(pretty_tree, model, pred_row, verbose) {
  
  #-----------------------------------------------------------------------------#
  # Function | get_decision_path
  #-----------------------------------------------------------------------------#
  # Layout   | Section 1. Get prediction route through tree
  #          | Section 2. Calculate feature contributions
  #-----------------------------------------------------------------------------#
  
  #-----------------------------------------------------------------------------#
  # Section 1. Get prediction route through tree ----
  #-----------------------------------------------------------------------------#
  
  if (verbose) {
    
    pred_route <- choose_split_r(row = 1,
                                 pretty_tree = pretty_tree, 
                                 model = model, 
                                 pred_row = pred_row)
    
  } else {
    
    suppressMessages(pred_route <- choose_split_r(row = 1,
                                                  pretty_tree = pretty_tree, 
                                                  model = model, 
                                                  pred_row = pred_row))
    
  }
  
  # change structure from list into data.frame
  pred_route_df <- do.call(rbind, pred_route)
  
  #-----------------------------------------------------------------------------#
  # Section 2. Calculate feature contributions ----
  #-----------------------------------------------------------------------------#
  
  # take differences i.e. contributions for each variable
  # note, the last element is the prediction for the
  # first node in the tree - this is only recorded here
  # so it can be easily summarised later
  pred_route_df$contrib <- c(diff(pred_route_df$prediction), 
                                  pred_route_df$prediction[1])
  
  return(pred_route_df)
  
}

