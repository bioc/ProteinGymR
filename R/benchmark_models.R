#' @rdname benchmark_models
#' 
#' @title Benchmark effect prediction models
#' 
#' @description `benchmark_models()` plots one of the five model performance 
#'    metrics ("AUC", "MCC", "NDCG", "Spearman", "Top_recall") for up to 
#'    5 user-specified variant effect prediction tools. See reference for more
#'    details.
#'    
#' @param metric `character()` one of the five model performance 
#'    metrics ("AUC", "MCC", "NDCG", "Spearman", "Top_recall").
#'    
#' @param models `character()` a character vector of up to five models assessed
#'    in ProteinGym.
#' 
#' @param alphamissense_table a table containing AlphaMissense predictions 
#'    for variants matching ProteinGym substitution mutants. The default is 
#'    the supplemental table from the AlphaMissense paper. 
#'    Alternatively, a user-defined [`tibble::tbl_df`] or [`data.frame`]
#'    can be supplied.
#'
#' @param dms_table a table containing deep mutational scanning (DMS) 
#'    assay scores for mutations. The default table loads substitutions from 
#'    [ProteinGym](https://proteingym.org/download).
#'    Alternatively, a user-defined [`tibble::tbl_df`] or [`data.frame`]
#'    can be supplied.
#'
#' @details
#'
#' For `ProteinGym_correlation_plot()`, 
#'    `alphamissense_table` columns must include:
#'
#' - `UniProt_id`: UniProt accession identifier.
#' - `mutant`: Mutant identifier string matching the dms_table format. 
#'    Protein position in the middle, and the reference and mutant 
#'    amino acid residues to the left and right of the position, respectively.
#' - `AlphaMissense`: AlphaMissense pathogenicity score.
#'
#'
#' `dms_table` columns must include:
#'
#' - `UniProt_id`: UniProt accession identifier.
#' - `mutant`: Mutant identifier string matching AlphaMissense variants. 
#'    Specifically, the set of substitutions to apply on the reference sequence 
#'    to obtain the mutated sequence (e.g., A1P:D2N implies the amino acid 'A' 
#'    at position 1 should be replaced by 'P', and 'D' at position 2 should be 
#'    replaced by 'N').
#' - `DMS_score`: Experimental measurement in the DMS assay. 
#'    Higher values indicate higher fitness of the mutated protein.
#'
#' @return `ProteinGym_correlation_plot()` returns a `ggplot` object visualizing 
#'    the Spearman correlation between experimental DMS scores and AlphaMissense 
#'    predicted scores and prints the r and p-value of the analysis to console. 
#'    Generally, a stronger negative correlation corresponds to a tighter 
#'    relationship between the two measures.
#'
#' @examples
#' 
#' ProteinGym_correlation_plot(uniprotId = "Q9NV35")
#' 
#' 
#' @references Cheng et al.,
#' Accurate proteome-wide missense variant effect prediction with AlphaMissense.
#' \emph{Science} 381, eadg7492. DOI:10.1126/science.adg7492.
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
#' 
#' 
#' 
#' 
benchmark_models <- # The user can specify, otherwise NULL = default 
    function(
        metric = c("AUC", "MCC", "NDCG", "Spearman", "Top_recall"), 
        models = c("AlphaMissense", "DeepSequence", "ESM")) {
    
            print(metric)
            print(models)
    
    }
 