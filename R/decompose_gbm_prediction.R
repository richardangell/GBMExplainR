#' Decompose gbm prediction into feature contributions + bias
#' 
#' For a single observation decompose the prediction for a gbm into feature 
#' contributions + bias. Within a single tree, the contribution for a given 
#' node is calculated by subtracting the prediction for the current node from 
#' the prediction of the next node the observation would visit in the tree. 
#' The predicted value for the first node in the tree is combined into the 
#' bias term (which also includes the intercept or \code{initF} from the model).
#' Node contributions are summed by the split variable for the node, across all 
#' trees in the model, giving the observation's prediction represented as
#' bias + contribution for each feature used in the model.
#' 
#' @param gbm \code{gbm.object} to predict with. Note multinomial distribution
#' gbms not currently supported.
#' @param prediction_row single row \code{data.frame} to predict and the 
#' decompose into feature contributions
#' @param type either "link" or "response". Default is "link". If "response"
#' and the gbm distribution is "poisson" then contributions are converted
#' to be on the response scale (i.e. counts). For all distributions except
#' "poisson" both options do the same.
#' @param verbose should split decisions be printed to console? Default value
#' is \code{FALSE}.
#' @param aggregate_contributions should feature contributions aggregated to
#' variable level be returned? Default is \code{TRUE}. The option is there to
#' inspect the contributions at tree x node level, which is mainly used with
#' the \code{validate_decomposition} function. Note, if contributions are not
#' aggregated then the model intercept will not be accounted for.
#' @param n_trees the number of trees to use in generating the prediction for
#' the given row. Default \code{NULL} uses all trees in the model.
#'   
#' @return \code{data.frame} containing variable contributions to predicted 
#' value. \cr
#' If \code{aggregate_contributions} = \code{TRUE}, the contributions are
#' at the variable level;
#'   \item{\code{variable}}{variable name}
#'   \item{\code{contribution}}{variable contribution to prediction}
#'   \item{\code{variable_value}}{value of variable for input row}
#'   \item{\code{variabel_class}}{class of variable}
#' If \code{aggregate_contributions} = \code{FALSE}, the contributions are   
#' at node x tree level, see output from \code{\link{get_decision_path}}.
#'   
#' @details Based on treeinterpreter Python package for random forests; 
#' \url{https://github.com/andosa/treeinterpreter}.  
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
decompose_gbm_prediction <- function(gbm, prediction_row, type = "link", 
                                     verbose = FALSE, 
                                     aggregate_contributions = TRUE,
                                     n_trees = NULL) {
  
  #----------------------------------------------------------------------------#
  # Function Layout
  # Section 0. Input checking
  # Section 1. Input checking
  # Section 2. Determine feature contributions
  # Section 3. Return feature contributions
  #----------------------------------------------------------------------------#
  
  #----------------------------------------------------------------------------#
  # Section 0. Input checking ----
  #----------------------------------------------------------------------------#
  
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
      
      # do not check categorical value if it is "NA"
      if (!is.na(cat_value) && cat_value != "NA") {
        
        if (!cat_value %in% gbm$var.levels[i][[1]]) {
          
          stop(paste(sQuote(cat_value), 
                     "is not an expected level for",
                     gbm$var.names[i]))  
          
        }
        
      }

    }
    
  }
  
  if (!aggregate_contributions) {
    
    warning("aggregate_contributions is FALSE, so contributions will be ", 
            "returned at node level not overall model level. This is mainly ",
            "used in conjunction with validate_decomposition. Model intercept ",
            "will not be included in this output.")
    
  }
  
  if (!type %in% c("link", "response")) {
    
    stop("type should be ", sQuote("link"), " or ", sQuote("response"))
    
  }
  
  if (gbm$distribution$name == "bernoulli") {
    
    if (type == "response") {
      
      stop("for ",
           sQuote("bernoulli"),
           " ",
           "distribution models it is only possible to return contributions on",
           " the link (logit) scale.")
      
    }
  
  } else if (gbm$distribution$name == "poisson") {
    
    if (type == "response") {
      
      warning("returning contributions on response scale (i.e. counts)")
      
    } else if (type == "link") {
      
      warning("returning contributions on link (log) scale")
      
    }
  
  } else if (gbm$distribution$name == "multinomial") {
    
    stop("multinomial distribution not currently supported")
    
  } else {
    
    if (type == "response") {
      
      warning("type of ", sQuote("response"), " only changes contributions ",
              "to response scale for the poisson distribution it will have no ",
              "effect for the ", gbm$distribution$name, " distribution.")
      
    }

  }
  
  # check n_trees is less than or equal to the number of trees in the model
  if (!is.null(n_trees)) {
    
    if (n_trees > gbm$n.trees) {
      
      stop("n_trees must be less than or equal to number of trees in the model")
      
    } else if (n_trees < 0)  {
      
      stop("n_trees must be greater than 0")
      
    }
    
  } else {
    
    n_trees <- gbm$n.trees
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 1. Input checking ----
  #----------------------------------------------------------------------------#
  
  explanatory_vars <- gbm$var.names
  
  model_intercept <- gbm$initF
  
  # loop through all trees in the model and get the decision route through 
  # each tree for the prediction row
  tree_routes <- lapply(1:n_trees,
    function(x) {
      pretty_tree <- pretty.gbm.tree(gbm, i.tree = x)
      
      tree_route <- get_decision_path(pretty_tree = pretty_tree,
                                      model = gbm, 
                                      pred_row = prediction_row,
                                      verbose = verbose)
      
      tree_route$tree_no <- x
      
      return(tree_route)
    }
  )

  #----------------------------------------------------------------------------#
  # Section 2. Determine feature contributions ----
  #----------------------------------------------------------------------------#
  
  # append routes through all trees into one data.frame
  tree_routes_all <- do.call(rbind, tree_routes)
  
  levels(tree_routes_all$variable) <- c(levels(tree_routes_all$variable),
                                        "Bias")
  
  tree_routes_all$variable[tree_routes_all$direction == "TerminalNode"] <- 
    "Bias"
  
  contributions <- aggregate(x = tree_routes_all$contrib,
                             by = list(tree_routes_all$variable),
                             FUN = sum)
  
  colnames(contributions) <- c("variable", "contribution")
  
  # add on the intercept to the bias term
  contributions[contributions$variable == "Bias", "contribution"] <- 
    contributions[contributions$variable == "Bias", "contribution"] + gbm$initF
  
  #----------------------------------------------------------------------------#
  # Section 3. Convert contributions to response scale for poisson ----
  #----------------------------------------------------------------------------#
  
  if (gbm$distribution$name == "poisson" & type == "response") {
    
    contributions$contribution <- exp(contributions$contribution)
    
  }
   
  #----------------------------------------------------------------------------#
  # Section 4. Add variable value and type  ----
  #----------------------------------------------------------------------------#
  
  if (aggregate_contributions) {
    
    cols <- 
      as.character(contributions$variable[contributions$variable != "Bias"])
    
    cols_row <- prediction_row[ , cols]
    
    # get column values in a single column as character
    col_values <- t(cols_row)[ , 1]
    
    col_classes <- sapply(cols_row, class)
    
    # add on NA for the bias row
    contributions$variable_value <- c(col_values, NA)
    
    contributions$variable_class <- c(col_classes, NA)
    
  }
  
  #----------------------------------------------------------------------------#
  # Section 5. Return feature contributions ----
  #----------------------------------------------------------------------------#
  
  if (aggregate_contributions) {
    
    return(contributions)
    
  } else {
    
    return(tree_routes_all)
    
  }
  
}


