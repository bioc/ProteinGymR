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
# Average model performance scores for plotting
avg_model_scores <- function(DMS_table, metric){
    
    # Spearman -- Absolute value
    if (metric == "Spearman"){ # -1 neg corr, 1 pos corr. Take abs value
        
        res <- DMS_table |> 
            select(2:length(DMS_table)) 
        res <- abs(res)
        res <- cbind(DMS_table$DMS_ID, res)
        colnames(res)[1] <- "DMS_ID"
        
    } else if (metric == "AUC"){ # 0 to 1
        res <- DMS_table
    } else if (metric == "MCC"){ # -1 worst; 1 best
        res <- DMS_table
    } else if (metric == "NDCG") { # 0 to 1
        res <- DMS_table
    } else # topk 0 to 1
        res <- DMS_table
    res
}

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
#'     scale_fill_continuous theme_classic annotate theme geom_boxplot
#'     
#' @importFrom dplyr select
#' 
#' @importFrom tidyr pivot_longer
#' 
#' @importFrom ggdist stat_halfeye stat_dots
#' 
#' @importFrom gghalves geom_half_point
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
    
    # Prepare scores for plotting
    res <- selected_table |> 
            select(2:length(selected_table)) 
    
    res <- abs(res)
    
    res_long <- res |> 
        pivot_longer(cols = everything(), 
               names_to = "model", 
               values_to = "score")
    
    # Reorder models in descending mean scores
    res_long <- res_long |> 
        group_by(model) |> 
        mutate(model_mean = mean(score)) |> 
        ungroup() |> 
        mutate(model = fct_reorder(model, model_mean, .desc = TRUE))

    # Raincloud plot
    res_long |> 
        ggplot(aes(x = model, y = score, fill = model, group = model)) + 
        ggdist::stat_halfeye(
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
        
        # add justified jitter from the {gghalves} package
        gghalves::geom_half_point(
            side = "l", 
            range_scale = .4, 
            alpha = .2
        ) +
        
        # add theme and fonts
        coord_cartesian(clip = "off") +
        theme_minimal() +
        xlab("Models") +
        ylab(paste(metric, "score")) +
        theme_classic() +
        theme(
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            legend.title = element_blank(),
            legend.text = element_text(size = 11)
        ) 
        
        # # Add mean annotations
        # geom_text(
        #     data = order(unique(res_long$model_mean), decreasing = TRUE),
        #     aes(y = mean_value, label = sprintf("Mean: %.2f", mean_value)),
        #     vjust = -1,
        #     size = 3.5,
        #     fontface = "bold"
        # ) +
        # 
        # # Customize the plot
        # scale_fill_brewer(palette = "Set2")
}
    