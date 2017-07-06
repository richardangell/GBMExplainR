#' Determine decision path down tree
#' 
#' @param row row index of \code{pretty_tree} to determine prediction 
#'   direction
#' @param prett_tree
#' @param model
#' @param pred_row row contain prediction values to choose split direction
#' 
#' @export
get_decision_path <- function(pretty_tree, model, pred_row) {
  
  # call traverse_tree_r to recursively traverse through the tree
  pred_route <- choose_split_r(row = 1,
                               pretty_tree = pretty_tree, 
                               model = model, 
                               pred_row = pred_row)
  
  # change structure from list into data.frame
  pred_route_df <- do.call(rbind, pred_route)
  
  # take differences i.e. contributions for each variable
  # note, the last element is the prediction for the
  # first node in the tree - this is only recorded here
  # so it can be easily summarised later
  pred_route_df$contrib <- c(diff(pred_route_df$prediction), 
                                  pred_route_df$prediction[1])
  
  return(pred_route_df)
  
}

