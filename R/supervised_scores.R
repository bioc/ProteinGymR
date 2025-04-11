#' @rdname ProteinGym_Supervised_Scores
#' 
#' @title Load Semi-Supervised Model Predictions for Substitutions in 217 Assays
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#' 
#' @param fold_scheme Character, which validation folding scheme to load. 
#' Options include: "contiguous", "modulo", or "random". Default behavior loads 
#' "contiguous". For more information about the different folding schemes,
#' refer to the original publication.
#'
#' @details `supervised_scores()` loads prediction scores outputted by 
#' semi-supervised models run on the 217 DMS substitution assays.
#' 
#' For raw model predictions, each assay includes 18 columns:
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
#' \item{\code{Columns 7:18}:}{Respective semi-supervised model name.}  
#'}
#'
#' @return Returns a [list()] object of 217 individual assays.
#' 
#' @examples
#' data <- supervised_scores()
#' data_random <- supervised_scores(fold_scheme = "random")
#' 
#' @export
supervised_scores <- function (metadata = FALSE, fold_scheme = "contiguous")
{
    
    # Check if fold_scheme is one of the allowed values
    valid_schemes <- c("contiguous", "modulo", "random")
  
    if (!(fold_scheme %in% valid_schemes)) {
        stop(sprintf("Invalid fold_scheme: '%s'. Must be one of: %s", 
            fold_scheme, paste(valid_schemes, collapse = ", ")))
    }
    
    # Check for metadata argument
    if (metadata == TRUE) {
        #eh[ehid]
        message("Grabbing metadata.")
    } else {
    
        # Load respective dataset
        if (fold_scheme == "contiguous") {
            message(
                c("Loading semi-supervised model scores ", 
                "with contiguous folding scheme")
            )
            data <- readRDS("../ProteinGym_data/EH_data/v1.2/supervised_contiguous5_scores_v1.2.rds")
            return(data)
        }   
        else if (fold_scheme == "modulo") {
            message(
                c("Loading semi-supervised model scores ", 
                "with modulo folding scheme")
            )
            data <- readRDS("../ProteinGym_data/EH_data/v1.2/supervised_modulo5_scores_v1.2.rds")
            return(data)
        } 
        else {
            message(
                c("Loading semi-supervised model scores ", 
                "with random folding scheme")
            )
            data <- readRDS("../ProteinGym_data/EH_data/v1.2/supervised_random5_scores_v1.2.rds")
            return(data)
        }
    }
}

#' @rdname ProteinGym_Supervised_Scores
#' 
#' @title Load Semi-Supervised Model Summary Metrics
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#'
#' @details `supervised_metrics()` loads in model performance summary metrics 
#' ("Spearman" and "MSE") from semi-supervised models in ProteinGymR run on 
#' 217 DMS substitution assays.
#' 
#' A metric summary table with 7 columns:
#' \describe{
#' \item{\code{UniProt_id}:}{Character, UniProt accession identifier.}
#' \item{\code{DMS_id}:}{Character, ProteinGym assay identifier.}
#' \item{\code{mutant}:}{Character, set of substitutions to apply on the 
#'    reference sequence to obtain the mutated sequence (e.g., A1P:D2N implies 
#'    the amino acid 'A' at position 1 should be replaced by 'P', and 'D' at 
#'    position 2 should be replaced by 'N').}
#' \item{\code{model_name}:}{Character, semi-supervised model used.}
#' \item{\code{fold_variable_name}:}{Character, the folding scheme used.}
#' \item{\code{Spearman}:}{Numeric, Spearman performance metric.}
#' \item{\code{MSE}:}{Numeric, MSE the Spearman performance metric.}
#'}
#'
#' @return Returns a [data.frame()] with 7 columns.
#' 
#' @examples
#' data <- supervised_metrics()
#' data_meta <- supervised_metrics(metadata = TRUE)
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
#' @export
supervised_metrics <- function (metadata = FALSE)
{
    # Check for metadata argument
    if (metadata == TRUE) {
        #eh[ehid]
        message("gathering metadata")
    }
    else {
        data <- readRDS("../ProteinGym_data/EH_data/v1.2/supervised_random5_scores_v1.2.rds")
        return(data)
    }
}