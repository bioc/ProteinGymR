#' @rdname benchmark_models
#' 
#' @title Benchmark effect prediction models
#' 
#' @description `benchmark_models()` plots one of the five model performance 
#'    metrics ("AUC", "MCC", "NDCG", "Spearman", "Top_recall") for up to 
#'    5 user-specified variant effect prediction tools. See reference for more
#'    details about the metrics and models. 
#'    
#' @param metric `character()` the model performance metric to
#'    compare ("AUC", "MCC", "NDCG", "Spearman", "Top_recall").
#'    
#' @param models `character()` a character vector listing up to five effect
#'    prediction models to compare. If no model specified, the top five highest
#'    performing models are displayed.
#'
#' @details
#'
#' @return `benchmark_models()` returns a `ggplot` object visualizing a chosen
#'    model performance metric between several variant effect prediction models.
#'
#' @examples
#' 
#' benchmark_models(metric = "Spearman", models = c("AlphaMissense", "EVE", 
#' "DeepSequence")
#' 
#' @references Notin, P., Kollasch, A., Ritter, D., van Niekerk, L., Paul, S., 
#' Spinner, H., Rollins, N., Shaw, A., Orenbuch, R., Weitzman, R., Frazer, J., 
#' Dias, M., Franceschi, D., Gal, Y., & Marks, D. (2023). 
#' ProteinGym: Large-Scale 
#' Benchmarks for Protein Fitness Prediction and Design. In A. Oh, T. Neumann, 
#' A. Globerson, K. Saenko, M. Hardt, & S. Levine (Eds.), \emph{Advances in 
#' Neural Information Processing Systems} (Vol. 36, pp. 64331-64379). 
#' Curran Associates, Inc.
#' 
#' @importFrom ggplot2 ggplot geom_bin2d aes element_text labs xlab ylab
#'     scale_fill_continuous theme_classic annotate theme
#' 
#' @export
benchmark_models <- function(
        metric = NULL, 
        models = NULL
){
    
    # Valid argument check function
    # check_argument()
    
    # Check valid metric argument
    valid_metric <- c("AUC", "MCC", "NDCG", "Spearman", "Top_recall")
    valid_model <- c()
    
    
    
}
        
    ) {

       
  
  # Default behavior: use first three models if no argument is provided
  if (is.null(model)) {
    model <- valid_options[1:3]
    cat("No models specified. Using default models:", paste(model, collapse = ", "), "\n")
  } else {
    # Check if provided models are valid
    if (!all(model %in% valid_options)) {
      invalid_models <- model[!model %in% valid_options]
      stop(paste("Invalid model(s) specified:", paste(invalid_models, collapse = ", ")))
    }
    
    # Check if number of models is within limit
    if (length(model) > 5) {
      stop("You can select up to 5 models")
    }
  }
  
        
    
    }
 