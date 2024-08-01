#' Load ProteinGym DMS Substitutions Scores
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
#' ((2022))[https://www.ncbi.nlm.nih.gov/pmc/articles/PMC10723403/].
#'
#'
#' Each assay includes 6 columns:
#' \describe{
#' \item{\code{UniProt_id}:}{Character, UniProt accession identifier.}
#' \item{\code{DMS_id}:}{Character, ProteinGym assay identifier.}
#' \item{\code{mutant}:}{Character, number of RNA features.}
#' \item{\code{mutated_sequence}:}{Factor, unique animal identifier.}
#' \item{\code{DMS_score}:}{Factor, batch identifier.}
#' \item{\code{DMS_score_bin}:}{Factor, young (YX) or old (OX) conditions of
#'                      the animal.}
#'}
#'
#'@author Tram Nguyen
#'
#' @references
#' Cheng et al. (2023)
#' Accurate proteome-wide missense variant effect prediction with AlphaMissense. 
#' \emph{Science} 391, eadg7492. DOI:10.1126/science.adg7492.
#' 
#' @examples
#' data <- ProteinGym_DMS_subs()
#' data_meta <- ProteinGym_DMS_subs(metadata = TRUE)
#' 
ProteinGym_DMS_subs <- function (metadata = FALSE)
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