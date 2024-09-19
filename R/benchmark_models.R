#' @rdname benchmark_models
#' 
#' @export
available_models <- function() 
    c("Site_Independent", "EVmutation", "DeepSequence_single",
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

#'
#' @noRd
# Check metric argument
check_metric_argument <- function(user_metric){

    ## Check if provided models are valid
    valid_metrics <- c("AUC", "MCC", "NDCG", "Spearman", "Top_recall")
        
    if (!all(user_metric %in% valid_metrics)) {
            
        invalid_metric <- user_metric[!user_metric %in% valid_metrics]
            
        stop(paste("Invalid model(s) specified:", invalid_metric))
    }
    
    ## Check that only one metric passed
    if (length(user_metric) > 1) {
        stop("Select only one metric for comparison")
    }
}

#'    
#' @noRd
# Check models argument
check_model_argument <- function(models){
    
    ## Check whether model is valid
    valid_models <- available_models()
    
    if (!all(models %in% valid_models)) {
        
        invalid_models <- models[!models %in% valid_models]
        
        stop(paste("Invalid model(s) specified:", invalid_models))
    }

    ## Check if number of models is within limit
    if (length(models) > 5) {
        stop("Select up to 5 models for comparison")
    }
}
#' 
#' @title Benchmark Variant Effect Prediction Models
#' 
#' @description `benchmark_models()` plots one of the five model performance 
#'    metrics ("AUC", "MCC", "NDCG", "Spearman", "Top_recall") for up to 
#'    5 user-specified variant effect prediction tools listed in 
#'    `available_models()`. See reference for more details about the metrics 
#'    and models. 
#'    
#' @param metric `character()` a model performance metric to
#'    benchmark ("AUC", "MCC", "NDCG", "Spearman", "Top_recall").
#'    
#' @param models `character()` a character vector of up to five variant effect
#'    prediction models to compare. Valid models can be seen with 
#'    `available_models()`.
#'
#' @return `benchmark_models()` returns a `ggplot` object visualizing a chosen
#'    model performance metric across several variant effect prediction models, 
#'    ordered by highest to lowest mean performance score. 
#'
#' @examples
#' # Currently support models
#' available_models()
#' 
#' benchmark_models(metric = "Spearman", models = c("Site_Independent", 
#' "DeepSequence_single", "ESM2_15B", "GEMME", "CARP_640M"))
#' 
#' benchmark_models(models = "GEMME")
#' 
#' @references Notin, P., Kollasch, A., Ritter, D., van Niekerk, L., Paul, S., 
#' Spinner, H., Rollins, N., Shaw, A., Orenbuch, R., Weitzman, R., Frazer, J., 
#' Dias, M., Franceschi, D., Gal, Y., & Marks, D. (2023). 
#' ProteinGym: Large-Scale Benchmarks for Protein Fitness Prediction and 
#' Design. In A. Oh, T. Neumann, A. Globerson, K. Saenko, M. Hardt, & 
#' S. Levine (Eds.), \emph{Advances in Neural Information Processing Systems} 
#' (Vol. 36, pp. 64331-64379). Curran Associates, Inc.
#'     
#' @importFrom dplyr select mutate group_by ungroup
#' 
#' @importFrom tidyr pivot_longer
#' 
#' @importFrom forcats fct_reorder
#' 
#' @importFrom ggplot2 ggplot coord_cartesian element_text scale_fill_discrete
#'     theme_classic annotate theme geom_boxplot element_blank ylab aes
#'     
#' @importFrom ggdist stat_halfeye stat_dots
#' 
#' @importFrom gghalves geom_half_point
#' 
#' @importFrom spdl info
#' 
#' @export
benchmark_models <- function(
    metric = c("AUC", "MCC", "NDCG", "Spearman", "Top_recall"),
    models = available_models()){

    ## If metric not provided, use Spearman
    if (missing(metric)){
        message("No metric specified. Using default Spearman correlation")
        metric <- "Spearman"
    } else {
        check_metric_argument(user_metric = metric)
    }
    
    ## If model not provided, give error
    if (missing(models)) {
        stop("Select at least one model from `available_models()`")
    } else {
    check_model_argument(models = models)
    }
    
    ## Load in benchmark scores
    metric_tables <- zeroshot_DMS_metrics()
    
    ## Pull relevant metric and models
    selected_table <- metric_tables[[metric]]
    selected_table <- selected_table |> select(all_of(models))
    
    ## If Spearman, take absolute value for plotting
    if (metric == "Spearman"){
        res <- abs(selected_table)
    } else {
        res <- selected_table
    }
    
    res_long <- res |> 
        pivot_longer(cols = everything(), 
               names_to = "model", 
               values_to = "score")
    
    ## Reorder models in descending mean scores
    res_long <- res_long |> 
        group_by(model) |> 
        mutate(model_mean = mean(score)) |> 
        ungroup() |> 
        mutate(model = fct_reorder(model, model_mean, .desc = TRUE))

    ## Raincloud plot
    res_long |> 
        ggplot(aes(x = model, y = score, fill = model, group = model)) + 
        stat_halfeye(
            adjust = .5, 
            width = .6, 
            .width = 0, 
            justification = -.2, 
            point_colour = NA
        ) + 
        geom_boxplot(
            width = .15, 
            outlier.shape = NA
        ) +
        geom_half_point(
            side = "l", 
            range_scale = .4, 
            alpha = .2
        ) +
        ## add theme and fonts
        coord_cartesian(clip = "off") +
        scale_fill_discrete(name = "Models") +
        theme_classic() +
        ylab(paste(metric, "score")) +
        theme(
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_blank(),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 11)
        )
}