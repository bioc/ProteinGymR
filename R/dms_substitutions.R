#' ProteinGym Deep Mutational Scanning (DMS) Scores for Substitutions
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#'
#' @return 
#' Returns a [list()] object of 216 individual assays.
#' 
#' @details 
#' This function loads in the ProteinGym deep mutational scanning assays (DMS) 
#' scores for substitutions in 216 studies. The data is provided by Notin et. al
#' 2023. See reference for details.
#'
#' Each assay includes 6 columns:
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
#'}
#'
#' @author Tram Nguyen
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
#' data <- dms_substitutions()
#' data_meta <- dms_substitutions(metadata = TRUE)
#' 
#' @export
dms_substitutions <- function (metadata = FALSE)
{
    eh <- ExperimentHub::ExperimentHub()
    title <- "ProteinGymR"
    
    eh <- AnnotationHub::query(eh, title)
    ehid <- "EH9555"
    
    if (metadata == TRUE) {
        eh[ehid]
    }
    else eh[[ehid]]
}