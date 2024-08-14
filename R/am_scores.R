#' AlphaMissense scores for ProteinGym variants
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#'
#' @return 
#' Returns a [data.frame()] object.
#' 
#' @details 
#' This function loads in the AlphaMissense information for substitutions 
#' matching those in the ProteinGym DMS assays. The table is taken from the 
#' AlphaMissense Supplementary Data by Cheng et al. 2023. See reference for 
#' details.
#'
#' The columns contain:
#' \describe{
#' \item{\code{DMS_id}:}{Character, ProteinGym assay identifier.}
#' \item{\code{Uniprot_ID}:}{Character, UniProt accession identifier.}
#' \item{\code{variant_id}:}{Character, variant identifier string matching 
#'    ProteinGym. Protein position in the middle, and the reference and mutant 
#'    amino acid residues to the left and right of the position, respectively.}
#' \item{\code{AlphaMissense}:}{Numeric, AlphaMissense pathogenicity score.}
#'}
#'
#' @author Tram Nguyen
#'
#' @references
#' Cheng et al. (2023)
#' Accurate proteome-wide missense variant effect prediction with AlphaMissense. 
#' \emph{Science} 391, eadg7492. DOI:10.1126/science.adg7492.
#' 
#' @examples
#' data <- am_scores()
#' data_meta <- am_scores(metadata = TRUE)
#' 
#' @export
am_scores <- function (metadata = FALSE)
{
    eh <- ExperimentHub::ExperimentHub()
    title <- "ProteinGymR"
    
    eh <- AnnotationHub::query(eh, title)
    ehid <- "EH9554"
    
    if (metadata == TRUE) {
        eh[ehid]
    }
    else eh[[ehid]]
}