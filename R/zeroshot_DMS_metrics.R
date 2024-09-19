#' @rdname zeroshot_DMS_metrics
#' 
#' @title Model performance metrics for DMS substitutions in the zero-shot 
#'    setting
#'
#' @param metadata Logical, whether only experiment metadata should be returned.
#' Default behavior is to return processed data with metadata included.
#' 
#' @details `zeroshot_DMS_metrics()` loads in the five model performance metrics 
#'    for ("AUC", "MCC", "NDCG", "Spearman", "Top_recall") calculated on the 
#'    DMS substitutions in the zero-shot setting.
#'
#' Each data.frame columns contain:
#' - "DMS_ID": Showing the assay name for the 217 DMS studies.
#' - Columns 2:63: Corresponding to the average performance score of each of the
#'    61 models tested.
#' - "Number_of_Mutants": Number of protein mutants evaluated. 
#' - "Selection_Type": Protein function grouping.
#' - "UniProt_ID": UniProt protein entry name identifier
#' - "MSA_Neff_L_category": Multiple sequence alignment category. 
#' - "Taxon": taxon group.
#'
#' @return Returns a [list()] object with five [data.frame()] corresponding to 
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
    eh <- ExperimentHub::ExperimentHub()
    ehid <- "EH9593"
    
    if (metadata == TRUE) {
        eh[ehid]
    }
    else eh[[ehid]]
}