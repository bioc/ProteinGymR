#' @rdname supervised_scores
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
#' @details `supervised_substitutions()` loads prediction scores outputted by 
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
#' data <- supervised_substitutions()
#' data_random <- supervised_substitutions(fold_scheme = "random")
#' meta <- supervised_substitutions(metadata = TRUE)
#' 
#' @export
supervised_substitutions <- function(metadata = FALSE, 
    fold_scheme = c("contiguous", "modulo", "random")) 
{
    
    # Check if fold_scheme was missing and set default with a message
    if (missing(fold_scheme)) {
        message("No fold_scheme specified, using contiguous scheme as default.")
    }

    # Match the argument to allowed choices and set default
    fold_scheme <- match.arg(fold_scheme)
    
    # Load EH
    eh <- ExperimentHub::ExperimentHub()
    
    if (isTRUE(metadata)) {
        message("Grabbing metadata only.")
        
        ehids <- c("EH9646", "EH9647", "EH9648")
        metadata_results <- lapply(ehids, function(id) eh[[id]])
        return(metadata_results)
        
    } else {
        ehid <- switch(
            fold_scheme,
            "contiguous" = "EH9646",
            "modulo"     = "EH9647",
            "random"     = "EH9648"
        )
        
        message(
            sprintf(
                "Loading semi-supervised model scores with %s folding scheme", 
                fold_scheme
            )
        )
        
        data <- eh[[ehid]]
        return(data)
    }
}


#' @rdname supervised_scores
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
#' meta <- supervised_metrics(metadata = TRUE)
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
    # Load EH
    eh <- ExperimentHub::ExperimentHub()
    
    # Check for metadata argument
    if (metadata == TRUE) {
        eh["EH9649"]
        message("Grabbing metadata only.")
    }
    else {
        data <- eh[["EH9649"]]
        return(data)
    }
}