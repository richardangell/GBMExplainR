#' Choose split direction (recursively)
#' 
#' This function sends the input observation (\code{pred_row}) down a tree to
#' a terminal node. Predicted values for all nodes visited are extracted. These
#' values are required to decompose a single prediction from the gbm into 
#' feature contributions plus bias.   
#' The tree structure must be provided in a \code{data.frame} output from
#' \code{pretty.gbm.tree}. The \code{gbm} model must also be given to provide 
#' variable names and types as well as categorical level names and split 
#' directions. 
#' 
#' @param row row index of \code{pretty_tree} to determine prediction 
#'   direction
#' @param prett_tree \code{data.frame} output from \code{pretty.gbm.tree}
#'   giving tree structure.
#' @param model \code{gbm.object} 
#' @param pred_row single \code{data.frame} row (containing explanatory columns) 
#'   to send down the tree to a terminal node
#' 
#' @return \code{list} of \code{data.frame} s showing split decisions for each 
#'   node visited by the given observation en route to a terminal node. 
#'   Contains columns;
#'   \item{\code{node_index}}{index of node observation has passed through}
#'   \item{\code{variable}}{name of the splitting variable (NA for terminal 
#'     nodes)}
#'   \item{\code{type}}{type for splitting variable, if type > 0 then the 
#'     variable is categorical otherwise it is ordered or continuous (NA for
#'     terminal nodes)}
#'   \item{\code{direction}}{child node to travel down from current node}
#'   \item{\code{prediction}}{prediction at current node}
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
#' # get deicsion path through tree 1 in gbm1 for first row in data
#' choose_split_r(row = 1,
#'                pretty_tree = pretty.gbm.tree(gbm1, i.tree = 1),
#'                model = gbm1,
#'                pred_row = data[1, ])
#' 
#' @export
choose_split_r <- function(row, pretty_tree, model, pred_row) {
  
  #----------------------------------------------------------------------------#
  # Function Layout
  # Section 1. If the current node is a terminal node
  # Section 2. Else if the current node is not a terminal node
  # Section 2.1. For categorical variables
  # Section 2.2. For continuous variables
  # Section 2.3. Otherwise error if var.type is negative
  # Section 3. Record split decision info in data.frame
  # Section 4. Return decision info or call choose_split_r again
  #----------------------------------------------------------------------------#
  
  message("running choose_split_r on row: ", row)
  
  split_var <- pretty_tree[row, "SplitVar"] 
  
  #----------------------------------------------------------------------------#
  # Section 1. If the current node is a terminal node ----
  #----------------------------------------------------------------------------#
  
  if (split_var == -1) {
    
    direction <- "TerminalNode"
    
    # splitting column name
    split_col_name <- NA
    
    message("split_col_name: ", split_col_name)
    
    # splitting column type
    split_col_type <- NA
    
    message("split_col_type: ", split_col_type)
    
  #----------------------------------------------------------------------------#
  # Section 2. Else if the current node is not a terminal node ----
  #----------------------------------------------------------------------------#
    
  } else {
    
    # index of splitting column
    # note, gbm package starts SplitVar idx from 0
    split_col_id <- pretty_tree[row, "SplitVar"] + 1
    
    message("split_col_id: ", split_col_id)
    
    # splitting column name
    split_col_name <- model$var.names[split_col_id]
    
    message("split_col_name: ", split_col_name)
    
    # splitting column type
    split_col_type <- model$var.type[split_col_id]
    
    message("split_col_type: ", split_col_type)
    
    #--------------------------------------------------------------------------#
    # Section 2.1. For categorical variables ----
    #--------------------------------------------------------------------------#
    
    if (split_col_type > 0) {
      
      # get categorical level split directions
      cat_directions <- 
        model$c.splits[pretty_tree[row, "SplitCodePred"] + 1][[1]]
      
      # get character vector of categorical levels
      cat_levels <- model$var.levels[split_col_id][[1]]
      
      cat_value <- as.character(pred_row[[split_col_name]])
      
      message("split varaible value: ", cat_value)
      
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
      
      #------------------------------------------------------------------------#
      # Section 2.2. For continuous / ordered variables ----
      #------------------------------------------------------------------------#
      
    } else if (split_col_type == 0) {
      
      num_value <- as.numeric(pred_row[[split_col_name]])
      
      # correct zero based indexing on ordered factor levels (just taking 
      # numeric will start level indexes at 1)
      # do not check for NAs as this results in an error
      if (!is.na(num_value) && !num_value == pred_row[[split_col_name]]) {
        
        num_value <- num_value - 1
        
      }
      
      message("split varaible value: ", num_value)
      
      # if pred_row value for the split variable is NA
      # check NA condition first as it will error the other conditions
      if (is.na(num_value)) {
        
        direction <- "MissingNode"
      
      # if pred_row value for the split variable is greater than the split point
      } else if (num_value >= pretty_tree[row, "SplitCodePred"]) {
        
        direction <- "RightNode"  
        
      # if pred_row value for the split variable is less than or equal to the 
      # split point
      } else if (num_value < pretty_tree[row, "SplitCodePred"]) {
        
        direction <- "LeftNode"  
        
      } else {
        
        stop("errr")
        
      }
      
      #------------------------------------------------------------------------#
      # Section 2.3. Otherwise error if var.type is negative ----
      #------------------------------------------------------------------------#
      
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
  
  #----------------------------------------------------------------------------#
  # Section 3. Record split decision info in data.frame ----
  #----------------------------------------------------------------------------#
  
  decision_df <- data.frame(node_index = row - 1,
                            variable = split_col_name,
                            type = split_col_type,
                            direction = direction,
                            prediction = pretty_tree[row, "Prediction"])
  
  #----------------------------------------------------------------------------#
  # Section 4. Return decision info or call choose_split_r again  ----
  #----------------------------------------------------------------------------#
  
  message("prediction: ", pretty_tree[row, "Prediction"])
  
  message("direction: ", direction)
  
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
