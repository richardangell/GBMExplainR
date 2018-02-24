#' Plot tree structure
#' 
#' Helper function for \code{plot_tree}. Takes node structure output from 
#' \code{pretty.gbm.tree} which is 1 row per node showing child nodes for that
#' node and returns the parents of each node.
#' 
#' @param pretty_tree \code{data.frame} output from \code{pretty.gbm.tree}
#' function.
#' 
#' @return \code{data.frame} with columns;
#' \item{\code{parent}}{parent of node}
#' \item{\code{node}}{node of interest}  
#'  
#' @export
get_node_parents <- function(pretty_tree) {
  
  n_nodes <- nrow(pretty_tree)
  
  pretty_tree$node_id <- row.names(pretty_tree)
  
  t_cols <- t(pretty_tree[pretty_tree$SplitVar != -1, 
                          c("node_id", 
                            "LeftNode", 
                            "RightNode", 
                            "MissingNode")])
  
  parent_node_list <- apply(t_cols, 2, function(x) 
    data.frame(parent = rep(x[1], 3), node = x[-1]))
  
  parent_node_df <- do.call(rbind, parent_node_list)
  
  return(parent_node_df)
  
} 
