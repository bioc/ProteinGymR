#' Load AlphaMissense scores for ProteinGym variants
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#'
#' @return 
#' Returns a [data.frame()] object.
#' 
#' @details 
#' This function loads in the AlphaMissense information for variants in 
#' ProteinGym. Data table derived from the AlphaMissense publication by 
#' Cheng et al.
#' ([2023](https://www.science.org/doi/10.1126/science.adg7492)).

#' The columns contain:
#' \describe{
#' \item{\code{barcode}:}{Character, unique cell identifier.}
#' \item{\code{nCount_RNA}:}{Numeric, number of RNA transcripts.}
#' \item{\code{nFeature_RNA}:}{Integer, number of RNA features.}
#' \item{\code{animal}:}{Factor, unique animal identifier.}
#' \item{\code{batch}:}{Factor, batch identifier.}
#' \item{\code{animal_type}:}{Factor, young (YX) or old (OX) conditions of
#'                      the animal.}
#' \item{\code{percent_mito}:}{Numeric, percentage of mitochondrial content.}
#' \item{\code{percent_ribo}:}{Numeric, percentage of ribosomal content.}
#' \item{\code{cell_type}:}{Factor, cell type to which the cell was assigned.}
#' \item{\code{cell_ontology_class}:}{Factor, Cell Ontology label.}
#' \item{\code{cell_ontology_id}:}{Factor, Cell Ontology identifier}
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
#' data <- AlphaMissense_scores()
#' data_meta <- AlphaMissense_scores(metadata = TRUE)
#' 
AlphaMissense_scores <- function (metadata = FALSE)
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