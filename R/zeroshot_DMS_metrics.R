#' @rdname zeroshot_scores
#' 
#' @title Load Zero-shot Model Predictions and Metrics for Substitutions 
#' in 217 DMS Assays
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#' 
#' @details `zeroshot_DMS_metrics()` loads in the model performance metrics 
#'    ("AUC", "MCC", "NDCG", "Spearman", "Top_recall") calculated on the 
#'    DMS substitutions in the zero-shot setting for 79 models updated in 
#'    ProteinGym v1.2.
#'    
#' Each data.frame contains the following columns:
#' 
#' \describe{ 
#' \item{\code{DMS_ID}:}{Character, Assay name for the DMS study.}
#' \item{\code{Columns 2:80}:}{Numeric, Corresponding to the average performance
#'  score of each of the 79 models tested.}
#' \item{\code{Number_of_Mutants}:}{Numeric, Number of protein mutants 
#'    evaluated.}
#' \item{\code{Selection_Type}:}{Character, Protein function grouping.}
#' \item{\code{UniProt_ID}:}{Character, UniProt protein entry name identifier.}
#' \item{\code{DMS_score}:}{Numeric, experimental measurement in the DMS assay. 
#'    Higher values indicate higher fitness of the mutated protein.}
#' \item{\code{DMS_score_bin}:}{Factor, indicates whether the DMS_score is 
#'    above the fitness cutoff (1 is fit, 0 is not fit).}
#' \item{\code{MSA_Neff_L_category}:}{Multiple sequence alignment category.} 
#' \item{\code{Taxon}:}{Taxon group.}
#'}
#'
#' @return Returns a [list()] object with 5 [data.frame()] corresponding to 
#'    a model metric table.
#' 
#' @references
#' Notin, P., Kollasch, A., Ritter, D., van Niekerk, L., Paul, S., Spinner, H., 
#' Rollins, N., Shaw, A., Orenbuch, R., Weitzman, R., Frazer, J., Dias, M., 
#' Franceschi, D., Gal, Y., & Marks, D. (2023). ProteinGym: Large-Scale 
#' Benchmarks for Protein Fitness Prediction and Design. In A. Oh, T. Neumann, 
#' A. Globerson, K. Saenko, M. Hardt, & S. Levine (Eds.), Advances in Neural 
#' Information Processing Systems (Vol. 36, pp. 64331-64379). 
#' Curran Associates, Inc.
#' 
#' @examples
#' data <- zeroshot_DMS_metrics()
#' data_meta <- zeroshot_DMS_metrics(metadata = TRUE)
#' 
#' @export
zeroshot_DMS_metrics <- function (metadata = FALSE)
{
    ## updated to v1.2 79 models
    if (metadata == TRUE) {
        eh[ehid]
    }
    else  {
        data <- readRDS("../ProteinGym_data/EH_data/v1.2/zeroshot_summary_scores_v1.2.rds")
        return(data)
    }
}


#' @rdname zeroshot_scores
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#' 
#' @details `zeroshot_substitutions()` loads prediction scores outputted by 
#' models in the zero-shot setting evaluated on the 217 DMS substitution assays.
#' To examine all model options, run `available_models()`.
#'
#' For raw model predictions, each data.frame includes 85 columns:
#' \describe{
#' \item{\code{UniProt_id}:}{Character, UniProt accession identifier.}
#' \item{\code{DMS_id}:}{Character, ProteinGym assay identifier.}
#' \item{\code{mutant}:}{Character, set of substitutions to apply on the 
#'    reference sequence to obtain the mutated sequence (e.g., A1P:D2N implies 
#'    the amino acid 'A' at position 1 should be replaced by 'P', and 'D' at 
#'    position 2 should be replaced by 'N').}
#' \item{\code{mutated_sequence}:}{Character, full amino acid sequence for the 
#'    mutated protein.}
#' \item{\code{DMS_score}:}{Numeric, experimental measurement in the DMS assay. 
#'    Higher values indicate higher fitness of the mutated protein.}
#' \item{\code{DMS_score_bin}:}{Factor, indicates whether the DMS_score is 
#'    above the fitness cutoff (1 is fit, 0 is not fit).}
#' \item{\code{Columns 7:85}:}{Respective zero-shot model name.}  
#'}
#'
#' @return Returns a [list()] object of 217 individual assays.
#' 
#' @examples
#' data <- zeroshot_substitutions()
#' data_random <- zeroshot_substitutions(fold_scheme = "random")
#' 
#' @export
zeroshot_substitutions <- function (metadata = FALSE)
{
    # Check for metadata argument
    if (metadata == TRUE) {
        #eh[ehid]
        message("Grabbing metadata.")
    } else {
       data <- readRDS("../ProteinGym_data/EH_data/v1.2/zeroshot_scores_v1.2.rds")
       return(data)
    }
}