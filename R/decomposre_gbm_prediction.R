#' Decompose gbm prediction into feature contributions
#' 
#' more detailed explaination of function
#' 
#' @param gbm \code{gbm} object to predict with
#' @param prediction_row single row \code{data.frame} to predict and the 
#'   decompose into feature contributions
#'   
#' @details based on treeinterpreter https://github.com/andosa/treeinterpreter  
#' 
#' @example 
#' build gbm 
#' predict row
#' decompose prediction
#' 
#' @export
decompose_gbm_prediction <- function(gbm, prediction_row) {
  
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
                                                          pred_row = prediction_row)
                          
                          return(tree_route)
                          
                        })

  #-----------------------------------------------------------------------------#
  # Section 2. Determine feature contributions ----
  #-----------------------------------------------------------------------------#
  
  # append routes through all trees into one data.frame
  tree_routes_all <- do.call(rbind, tree_routes)
  
  levels(tree_routes_all$variable) <- c(levels(tree_routes_all$variable),
                                        "TreeStart")
  
  tree_routes_all$variable[tree_routes_all$direction == "TerminalNode"] <- "TreeStart"
  
  contributions <- aggregate(x = tree_routes_all$contrib,
                             by = list(tree_routes_all$variable),
                             FUN = sum)
  
  #-----------------------------------------------------------------------------#
  # Section 3. Return feature contributions ----
  #-----------------------------------------------------------------------------#
  
  return(contributions)
  
}


