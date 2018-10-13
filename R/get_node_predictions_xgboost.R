#' Get node predictions for xgboost model
#' 
#' @param xgb_model \code{xgb.Booster} to extract node predictions from ,
#' 
#' @return \code{data.table} with rows representing nodes in trees from the 
#' model and the following columns;
#'   \item{row}{data.table row number}
#'   \item{Tree}{tree index}
#'   \item{Node}{node index}
#'   \item{Feature}{variable used for splitting at node or "Leaf" for terminal 
#'   nodes}
#'   \item{Split}{split point for node}
#'   \item{Yes}{tree / node index for 'Yes' child node}
#'   \item{No}{tree / node index for 'No' child node}
#'   \item{Missing}{tree / node index for 'Missing' child node}
#'   \item{Quality}{Gain for node or weight if leaf node}
#'   \item{Cover}{first order derivative of loss function}
#'   \item{weight}{node predictions}
#'   \item{H}{data.table row number}
#'   \item{G}{Gain for node}
#'   \item{G_yes}{G value for the 'Yes' child node}
#'   \item{G_no}{G value for the 'No' child node}
#' Note the following columns are calculated; row, weight, H, G, G_yes and G_no.
#' The other columns are output from \code{xgboost::xgb.model.dt.tree}     
#'   
#' @examples
#' library(xgboost)
#' data(agaricus.train, package='xgboost')
#' data(agaricus.test, package='xgboost')
#' train <- agaricus.train
#' test <- agaricus.test
#'
#' bstSparse <- xgboost(data = train$data, 
#'                      label = train$label, 
#'                      max.depth = 1, 
#'                      eta = 1, 
#'                      nrounds = 2, 
#'                      objective = "binary:logistic")
#' 
#' get_node_predictions_xgboost(bstSparse)
#' 
#' @export
get_node_predictions_xgboost <- function(xgb_model) {
  
  if (!class(xgb_model) == "xgb.Booster") {
    
    stop("xgb_model should be xgb.Booster class")
    
  }
  
  xgb_trees <- xgboost::xgb.model.dt.tree(model = xgb_model)
  
  if ("base_score" %in% names(xgb_model$params)) {
    
    base_score <- xgb_model$params$base_score
    
  } else {
    
    base_score <- 0.5
    
  }
  
  if ("objective" %in% names(xgb_model$params)) {
    
    objective <- xgb_model$params$objective
    
  } else {
    
    objective <- "reg:linear"
    
  }
  
  if (objective %in% c("reg:logistic", "binary:logistic")) {
    
    base_score <- log(base_score / (1 - base_score))
    
  }
  
  # col to hold leaf prediction values
  xgb_trees[['weight']] <- 0
  
  xgb_trees[Feature == 'Leaf', weight := base_score + Quality] 
  
  xgb_trees[['H']] <- xgb_trees[['Cover']]
  
  xgb_trees[['G']] <- 0
  
  xgb_trees[Feature == 'Leaf', G := - weight * H] 
  
  # add row index
  #xgb_trees[ , row := 1:.N]
  xgb_trees[['row']] <- 1:nrow(xgb_trees)
  
  xgb_trees_G <- xgb_trees[ , c('ID', 'G'), with = F]
  
  setnames(xgb_trees_G, old = 'G', new = 'G_yes')
  
  xgb_trees <- merge(x = xgb_trees,
                     y = xgb_trees_G,
                     by.x = 'Yes',
                     by.y = 'ID',
                     all.x = TRUE)
  
  setnames(xgb_trees_G, old = 'G_yes', new = 'G_no')
  
  xgb_trees <- merge(x = xgb_trees,
                     y = xgb_trees_G,
                     by.x = 'No',
                     by.y = 'ID',
                     all.x = TRUE)[order(row)]
  
  xgb_trees[Feature != 'Leaf', G := G_yes + G_no]
  
  xgb_trees[Feature != 'Leaf', weight := - G / H]
  
  setcolorder(xgb_trees, c("row",
                           "Tree", 
                           "Node",
                           "ID",
                           "Feature",
                           "Split",
                           "Yes",
                           "No",
                           "Missing",
                           "Quality",
                           "Cover",
                           "weight",
                           "H",
                           "G",
                           "G_yes",
                           "G_no"))
  
  return(xgb_trees)
  
}









