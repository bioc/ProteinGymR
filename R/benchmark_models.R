#' @rdname benchmark_models
#' 
#' @noRd
# Check metric argument
check_metric_argument <- function(metric){
    
    # Check whether models specified or NULL
    if (is.null(metric)) {
        cat("No metric specified. Using default Spearman correlation. \n")
    } else {
    
        # Check if provided models are valid
        valid_metrics <- c("AUC", "MCC", "NDCG", "Spearman", "Top_recall")
        
        if (!all(metric %in% valid_metrics)) {
            
            invalid_metric <- metric[!metric %in% valid_metrics]
            
            stop(paste("Invalid model(s) specified:", 
            paste(invalid_metric, collapse = ", ")))
        }
    
        # Check that only metric passed
        if (length(metric) > 1) {
            stop("Select only one metric for comparison.")
        }
    }
}
#'    
#' @noRd
# Check models argument
check_model_argument <- function(models){
    
    # Check whether models specified or NULL
    if (is.null(models)) {
        cat("No models specified. Using default models. \n")
    } else {
    
    # Check if provided models are valid
    valid_models <- c("Site_Independent", "EVmutation", "DeepSequence_single",
        "DeepSequence_ensemble", "EVE_single", "EVE_ensemble", "Unirep", 
        "Unirep_evotuned", "MSA_Transformer_single", "MSA_Transformer_ensemble",
        "ESM_1b",  "ESM_1v_single", "ESM_1v_ensemble", "ESM2_8M", "ESM2_35M", 
        "ESM2_150M", "ESM2_650M", "ESM2_3B", "ESM2_15B", "Wavenet", 
        "RITA_S", "RITA_M", "RITA_L", "RITA_XL", "Progen2_S", "Progen2_M",
        "Progen2_Base", "Progen2_L", "Progen2_XL", "GEMME", "VESPA", "VESPAl", 
        "ProtGPT2", "Tranception_S_no_retrieval", "Tranception_M_no_retrieval", 
        "Tranception_L_no_retrieval", "Tranception_S", "Tranception_M", 
        "Tranception_L", "TranceptEVE_S", "TranceptEVE_M",  "TranceptEVE_L", 
        "CARP_38M", "CARP_600K", "CARP_640M", "CARP_76M", "MIF", "MIF_ST",
        "ESM_IF1", "ProteinMPNN", "ProtSSN_k_10_h_512", "ProtSSN_k_10_h_768", 
        "ProtSSN_k_10_h_1280", "ProtSSN_k_20_h_512", "ProtSSN_k_20_h_768", 
        "ProtSSN_k_20_h_1280", "ProtSSN_k_30_h_512", "ProtSSN_k_30_h_768", 
        "ProtSSN_k_30_h_1280", "ProtSSN_ensemble", "SaProt_650M", "SaProt_35M")
    
        if (!all(models %in% valid_models)) {
            
            invalid_models <- models[!models %in% valid_models]
            
            stop(paste("Invalid model(s) specified:", 
            paste(invalid_models, collapse = ", ")))
        }
    
        # Check if number of models is within limit
        if (length(models) > 5) {
            stop("Select up to 5 models for comparison.")
        }
    }
}

#'
#' @noRd
# Calculate AlphaMissense scores
#' calc_alphamissense_metric <- function(models){
    
    # Load in AlphaMissense. Needed for separate calculation
#    am_metric <- am_scores()
#} 
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
#' @return `benchmark_models()` returns a `ggplot` object visualizing a chosen
#'    model performance metric between several variant effect prediction models.
#'
#' @examples
#' 
#' benchmark_models(metric = "Spearman", models = c("Site_Independent", 
#' "DeepSequence_single", "ESM2_15B", "GEMME", "CARP_640M"))
#' 
#' @references Notin, P., Kollasch, A., Ritter, D., van Niekerk, L., Paul, S., 
#' Spinner, H., Rollins, N., Shaw, A., Orenbuch, R., Weitzman, R., Frazer, J., 
#' Dias, M., Franceschi, D., Gal, Y., & Marks, D. (2023). 
#' ProteinGym: Large-Scale Benchmarks for Protein Fitness Prediction and 
#' Design. In A. Oh, T. Neumann, A. Globerson, K. Saenko, M. Hardt, & 
#' S. Levine (Eds.), \emph{Advances in Neural Information Processing Systems} 
#' (Vol. 36, pp. 64331-64379). Curran Associates, Inc.
#' 
#' @importFrom ggplot2 ggplot geom_bin2d aes element_text labs xlab ylab
#'     scale_fill_continuous theme_classic annotate theme
#'     
#' @importFrom dplyr select
#' 
#' @export
benchmark_models <- function(
        metric = NULL, 
        models = NULL) {
    
    # Check valid arguments
    check_model_argument(models = models)
    check_metric_argument(metric = metric)
    
    # Load in benchmark scores
    metric_tables <- zeroshot_DMS_metrics()
    
    # Pull relevant metric and models
    selected_table <- metric_tables[[metric]]
    selected_table <- selected_table |> select(all_of(c("DMS_ID", models)))
    
    
}
