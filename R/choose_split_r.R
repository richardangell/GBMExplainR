#' Choose split direction (recursively)
#' 
#' @param row row index of \code{pretty_tree} to determine prediction 
#'   direction
#' @param prett_tree
#' @param model
#' @param pred_row row contain prediction values to choose split direction
#' 
#' @export
choose_split_r <- function(row, pretty_tree, model, pred_row) {
  
  #-----------------------------------------------------------------------------#
  # Function | choose_split_r
  #-----------------------------------------------------------------------------#
  # Layout   | Section 1. If the current node is a terminal node
  #          | Section 2. Else if the current node is not a terminal node
  #          | Section 2.1. For categorical variables
  #          | Section 2.2. For continuous variables
  #          | Section 2.3. Otherwise error if var.type is negative
  #          | Section 3. Record split decision info in data.frame
  #          | Section 4. Return decision info or call choose_split_r again
  #-----------------------------------------------------------------------------#
  
  cat("running choose_split_r on row:", row, "\n")
  
  split_var <- pretty_tree[row, "SplitVar"] 
  
  #-----------------------------------------------------------------------------#
  # Section 1. If the current node is a terminal node ----
  #-----------------------------------------------------------------------------#
  
  if (split_var == -1) {
    
    direction <- "TerminalNode"
    
    # splitting column name
    split_col_name <- NA
    
    cat("split_col_name:", split_col_name, "\n")
    
    # splitting column type
    split_col_type <- NA
    
    cat("split_col_type:", split_col_type, "\n")
    
  #-----------------------------------------------------------------------------#
  # Section 2. Else if the current node is not a terminal node ----
  #-----------------------------------------------------------------------------#
    
  } else {
    
    # index of splitting column
    # note, gbm package starts SplitVar idx from 0
    split_col_id <- pretty_tree[row, "SplitVar"] + 1
    
    cat("split_col_id:", split_col_id, "\n")
    
    # splitting column name
    split_col_name <- model$var.names[split_col_id]
    
    cat("split_col_name:", split_col_name, "\n")
    
    # splitting column type
    split_col_type <- model$var.type[split_col_id]
    
    cat("split_col_type:", split_col_type, "\n")
    
    #-----------------------------------------------------------------------------#
    # Section 2.1. For categorical variables ----
    #-----------------------------------------------------------------------------#
    
    if (split_col_type > 0) {
      
      # get categorical level split directions
      cat_directions <- model$c.splits[pretty_tree[row, "SplitCodePred"] + 1][[1]]
      
      # get character vector of categorical levels
      cat_levels <- model$var.levels[split_col_id][[1]]
      
      cat_value <- as.character(pred_row[[split_col_name]])
      
      # get the direction of the categorical value for pred_row
      cat_value_dir <- cat_directions[which(cat_levels == cat_value)]
      
      if (cat_value_dir == 1) {
        
        direction <- "RightNode"  
        
      } else if (cat_value_dir == -1) {
        
        direction <- "LeftNode"  
        
        # 0 indicates that the cat level was not present in the training data
      } else if (cat_value_dir == 0) {
        
        direction <- "TerminalNode"
        
      } else if (is.na(cat_value_dir)) {
        
        direction <- "LeftNode" 
        
      } else {
        
        stop("errr")
        
      }
      
      #-----------------------------------------------------------------------------#
      # Section 2.2. For continuous variables ----
      #-----------------------------------------------------------------------------#
      
    } else if (split_col_type == 0) {
      
      # if pred_row value for the split variable is greater than the split point
      if (pred_row[[split_col_name]] >= pretty_tree[row, "SplitCodePred"]) {
        
        direction <- "RightNode"  
        
        # if pred_row value for the split variable is less than or equal to the 
        # split point
      } else if (pred_row[[split_col_name]] < pretty_tree[row, "SplitCodePred"]) {
        
        direction <- "LeftNode"  
        
        # if pred_row value for the split variable is NA
      } else if (is.na(pred_row[[split_col_name]])) {
        
        direction <- "MissingNode"
        
      } else {
        
        stop("errr")
        
      }
      
      #-----------------------------------------------------------------------------#
      # Section 2.3. Otherwise error if var.type is negative ----
      #-----------------------------------------------------------------------------#
      
    } else {
      
      stop(paste(c("got split column type",
                   split_col_type,
                   "for",
                   split_col_name,
                   "on row",
                   row),
                 sep = " "))
      
    }
    
  }
  
  #-----------------------------------------------------------------------------#
  # Section 3. Record split decision info in data.frame ----
  #-----------------------------------------------------------------------------#
  
  decision_df <- data.frame(variable = split_col_name,
                            type = split_col_type,
                            direction = direction,
                            prediction = pretty_tree[row, "Prediction"])
  
  #-----------------------------------------------------------------------------#
  # Section 4. Return decision info or call choose_split_r again  ----
  #-----------------------------------------------------------------------------#
  
  cat("prediction:", pretty_tree[row, "Prediction"], "\n")
  
  cat("direction:", direction, "\n")
  
  # if the node is a terminal node return the results in df
  if (direction == "TerminalNode") {
    
    return(list(decision_df))
    
    # otherwise if the row goes left, right or to the unknown node then call
    # traverse_tree again
  } else {
    
    return(c(list(decision_df),
             choose_split_r(row = pretty_tree[row, direction] + 1, 
                            pretty_tree = pretty_tree, 
                            model = model, 
                            pred_row = pred_row)))
    
  }
  
}  
