#' Plot tree structure
#' 
#' Plot structure for given tree in gbm, with the option to display the path
#' to a terminal node for 1 row of data.
#' 
#' @param gbm \code{gbm.object} to predict with. 
#' @param tree_no specific tree to plot from the model.
#' @param plot_path single row \code{data.frame} to predict and the 
#' decompose into feature contributions. Default value \code{NULL} means not
#' terminal node path will be displayed. 
#' 
#' @return function does not return anything but plots tree structure.
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
#' plot_tree(gbm1, 1, data[1, ])
#' 
#' @export
plot_tree <- function(gbm, tree_no, plot_path = NULL, label_cex, ...) {
  
  #----------------------------------------------------------------------------#
  # Function Layout
  # Section 0. Input checking
  # Section 1. Extract tree structure
  # Section 2. Get node variable names
  # Section 3. Get node variable types
  # Section 4. Get continuous and categorical variables
  # Section 5. Get the left split condition for continuous variables
  # Section 6. Get the left split condition for categorical variables
  # Section 7. Combine terminal node predictions to get node text
  # Section 8. Get node parent structure
  # Section 9. Set up tree structure
  # Section 10. Set edge widths for terminal node path
  # Section 11. Plot tree structure
  #----------------------------------------------------------------------------#
  
  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#
  
  if (!is(gbm, "gbm")) {
    
    stop("gbm should be class gbm.")
    
  }
  
  if (!is.null(plot_path)) {
    
    if (!is(plot_path, "data.frame")) {
      
      stop("plot_path should be a data.frame")
      
    }
    
    if (nrow(plot_path) != 1) {
      
      stop("plot_path should be a single row data.frame")
      
    }
    
    supplied_columns <- colnames(plot_path)
    
    if (any(!gbm$var.names %in% supplied_columns)) {
      
      stop("the following columns required by the model are not present; ",
           paste(gbm$var.names[which(!gbm$var.names %in% supplied_columns)]))
      
    }
    
  }
  
  if (tree_no > gbm$n.trees) {
    
    stop("tree_no must be less than or equal to number of trees in the model")
    
  } else if (tree_no < 0)  {
    
    stop("tree_no must be greater than 0")
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 1. Extract tree structure ----
  #----------------------------------------------------------------------------#
  
  pretty_tree <- pretty.gbm.tree(object = gbm, i.tree = tree_no)
  
  pretty_tree$split_var <- NA
  pretty_tree$split_var_type <- NA
  pretty_tree$left_split_cond <- NA
  
  #----------------------------------------------------------------------------#
  # Section 2. Get node variable names ----
  #----------------------------------------------------------------------------#
  
  # note gbm stores variable from index 0 in pretty tree table
  # terminal nodes where SplitVar = -1 go to 0 which return nothing from vec
  pretty_tree$split_var[pretty_tree$SplitVar != -1] <- 
    gbm$var.names[pretty_tree$SplitVar + 1]
    
  #----------------------------------------------------------------------------#
  # Section 3. Get node variable types ----
  #----------------------------------------------------------------------------#

  # get gbm encoding of variable type
  # continuous variables have a value of 0
  # categorical variables contain the index of gbm$c.splits showing each
  #   categorical level direction
  pretty_tree$split_var_type[pretty_tree$SplitVar != -1] <- 
    gbm$var.type[pretty_tree$SplitVar + 1]
  
  #----------------------------------------------------------------------------#
  # Section 4. Get continuous and categorical variables ----
  #----------------------------------------------------------------------------#
  
  continuous_variables <- !is.na(pretty_tree$split_var_type) &
    pretty_tree$split_var_type == 0
  
  categorical_variables <- !is.na(pretty_tree$split_var_type) &
    pretty_tree$split_var_type > 0
    
  #----------------------------------------------------------------------------#
  # Section 5. Get the left split condition for continuous variables ----
  #----------------------------------------------------------------------------#
  
  if (any(continuous_variables)) {
    
    pretty_tree$left_split_cond[continuous_variables] <- 
      paste0(pretty_tree$split_var[continuous_variables], 
             " < ", 
             signif(pretty_tree$SplitCodePred[continuous_variables],
                    digits = 6))
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 6. Get the left split condition for categorical variables ----
  #----------------------------------------------------------------------------#
  
  if (any(categorical_variables)) {
    
    # extract categorical variable level split directions
    cat_splits <- 
      gbm$c.splits[pretty_tree$split_var_type[categorical_variables]]
    
    # loop through all categorical variables
    for (i in which(categorical_variables)) {
      
      cat_splits <- gbm$c.splits[[pretty_tree[i, "split_var_type"]]]
      
      cat_index <- pretty_tree[i, "SplitVar"] + 1
      
      cat_name <- pretty_tree[i, "split_var"]
      
      cat_levels <- gbm$var.levels[[cat_index]]
      
      if (length(cat_levels) != length(cat_splits)) {
        
        stop("mismatch between categorical split directions and number of ",
             "categorical levels")
        
      }
      
      left_levels <- cat_levels[cat_splits == -1]
      
      pretty_tree$left_split_cond[i] <- paste0(cat_name, 
                                               " in ",
                                               "\n",
                                               "(",
                                               paste(left_levels, 
                                                     collapse = ", "),
                                               ")")
      
    }

  }
  
  #----------------------------------------------------------------------------#
  # Section 7. Combine terminal node predictions to get node text ----
  #----------------------------------------------------------------------------#
  
  # this column becomes the text to display over the node
  pretty_tree$left_split_cond[pretty_tree$SplitVar == -1] <-
    signif(pretty_tree$SplitCodePred[pretty_tree$SplitVar == -1],
           digits = 6)
  
  #----------------------------------------------------------------------------#
  # Section 8. Get node parent structure ----
  #----------------------------------------------------------------------------#

  node_parents <- get_node_parents(pretty_tree)
  
  # create edge text; whether the node contion was satisfied or not or missing
  node_parents$text <- rep(c("Y", "N", "NA"), 
                           nrow(node_parents) / 3)
  
  #----------------------------------------------------------------------------#
  # Section 9. Set up tree structure ----
  #----------------------------------------------------------------------------#
  
  # structure 
  g <- igraph::graph.data.frame(node_parents)
  
  # get the default labels for nodes in graph (corresponds to gbm indices)
  original_vertex_labels <- as.numeric(igraph::V(g)$name)
  
  # select the left split condition column to display over nodes in order
  new_labels <- pretty_tree$left_split_cond[original_vertex_labels + 1]
  
  # remove arrows from edges
  igraph::E(g)$arrow.size <- 0
  
  plot_title <- paste0("GBM Tree ", tree_no, " Structure")
  
  #----------------------------------------------------------------------------#
  # Section 10. Set edge widths for terminal node path ----
  #----------------------------------------------------------------------------#
  
  if (!is.null(plot_path)) {
    
    # get route to terminal node
    terminal_node_path <- 
      get_decision_path(pretty_tree = pretty_tree, 
                        model = gbm,
                        pred_row = plot_path,
                        verbose = FALSE)   
    
    # get the terminal node (gbm index)
    terminal_node <- 
      terminal_node_path[terminal_node_path$direction == "TerminalNode", 
                         "node_index"]
    
    # get the terminal node index in the graph
    terminal_node_idx <- which(original_vertex_labels == terminal_node)

    # get the route through the graph to the terminal node
    terminal_node_path_graph <- 
      igraph::shortest_paths(g, 
                             from = igraph::V(g)[0],
                             to  = igraph::V(g)[terminal_node_idx],
                             output = "both")
    
    # set all edges to be equal width inititally
    edges_size <- rep(1, length(igraph::E(g)))
    
    # widen the edges travelled to the terminal node
    edges_size[unlist(terminal_node_path_graph$epath)] <- 10
    
    # set edge widths
    igraph::E(g)$width <- edges_size
    
    plot_title <- paste0("GBM Tree ", 
                         tree_no, 
                         " Structure with Terminal Node Path")
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 11. Plot tree structure ----
  #----------------------------------------------------------------------------#
  
  igraph::plot.igraph(g, 
                      layout = layout.reingold.tilford, 
                      edge.label= E(g)$text, 
                      vertex.label = new_labels,
                      ...)
  
  title(plot_title, cex.main = 2)
  
}

