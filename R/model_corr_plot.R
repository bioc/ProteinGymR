#' #' @rdname model_corr_plot
#' #'
#' #' @noRd
#' #' 
#' #' @importFrom dplyr filter pull as_tibble rename_with mutate case_when
#' #' 
#' #' @importFrom queryup query_uniprot
#' #' 
#' #' @importFrom ExperimentHub ExperimentHub
#' #'
#' 
#' # Correlate two model scores
#' 
#' #It also possible to adapt this function skeleton to correlate the DMS scores
#' #and any of the models available in ProteinGym. Below is the workflow to 
#' #accomplish this.
#' 
#' #First, we want to select a specific assay from the 217 studies that we'd like to
#' #assess. Download and load in model scores for all DMS variants. 
#' #Let's explore the supervised model scores for this example, but the same can be 
#' #done with the zero-shot scores as well. 
#' 
#' #Create a BiocFileCache and download the data into this.
#' 
#' # Create or load the Bioconductor file cache
#' #bfc <- BiocFileCache()
#' #show(bfc)
#' 
#' # Add the resource to the cache (does not download yet)
#' #savepath <- bfcnew(bfc, "supervised_scores", ext=".zip")
#' #savepath
#' 
#' url <- "https://zenodo.org/records/14997691/files/DMS_supervised_substitutions_scores.zip?download=1"
#' 
#' add <- bfcadd(bfc, "supervised_scores", fpath=url)
#' rid <- names(add)
#' 
#' # Get the actual path on disk for that resource ID
#' destfile <- bfcrpath(bfc, rids = rid)
#' 
#' # Now download the file into that destfile location
#' download.file(url, destfile = destfile, mode = "wb")
#' 
#' # Data stored locally
#' ProGym_files <- list.files(path=destfile, full.names=FALSE)
#' 
#' # Grab assay names
#' ProGym_names <- sub(".csv", "", ProGym_files) 
#' 
#' # Load in all the files as tables in a list
#' ProGym_tables <- suppressMessages(lapply(paste0(file_dir, ProGym_files),
#'                                          read_csv, show_col_types = FALSE))
#' 
#' 
#' 
#' 
#' # Load DMS scores
#' dms_data <- dms_substitutions()
#' 
#' 
#' 
#' 
#' 
#' 
#' assay_name <- "A0A140D2T1_ZIKV_Sourisseau_2019"
#' model <- "S3F_MSA"
#' 
#' # Filter selected assay
#' model_df <- model_data[[assay_name]]
#' dms_df <- dms_data[[assay_name]]
#' 
#' ## Filter out multiple aa sites
#' model_df <- model_df |>  
#'     filter(!grepl(":", .data$mutant))
#' 
#' dms_df <- dms_df |>  
#'     filter(!grepl(":", .data$mutant))
#' 
#' ## Select chosen model
#' model_df <- model_df |>
#'     dplyr::select(
#'         mutant,
#'         all_of(model)
#'     )
#' 
#' model_df <- model_df |>
#'     dplyr::select(all_of(model), mutant)
#'         
#' ## Wrangle the data
#' dms_df <- dms_df |>
#'    dplyr::select(DMS_score, mutant)
#' 
#' merged_table <- 
#'         left_join(
#'             model_df, dms_df, 
#'             by = c("mutant"),
#'             relationship = "many-to-many"
#'         ) |>
#'         na.omit() |> 
#'         select(mutant, DMS_score, model = all_of(model))
#' 
#' ## Corr function
#' pg_correlate <- 
#'     function(merged_table)
#' {
#'     cor_results <- 
#'         cor.test(
#'             merged_table$DMS_score, merged_table$model, 
#'             method=c("spearman"), 
#'             exact = FALSE
#'         )
#'     
#'     cor_results
#' }
#' 
#' cor_results <- pg_correlate(merged_table)
#' 
#' 
#' ## Correlation density plot
#' pg_density_plot <- 
#'     merged_table |> 
#'     ggplot(
#'         aes(x = .data$DMS_score, y = .data$model)
#'     ) +
#'     geom_bin2d(bins = 60) +
#'     scale_fill_continuous(type = "viridis") +
#'     xlab("DMS score") +
#'     ylab(paste0(model, "\nzero shot score")) +
#'     theme_classic() +
#'     theme(
#'         axis.text.x = element_text(size = 16),
#'         axis.text.y = element_text(size = 16),
#'         axis.title.y = element_text(size = 16, vjust = 2),
#'         axis.title.x = element_text(size = 16, vjust = 0),
#'         legend.title = element_text(size = 16),
#'         legend.text = element_text(size = 16)
#'     )
#' 
#' pg_density_plot
#' 
#' print(paste0("r = ", format(round(cor_results$estimate, 2)), 
#'                 "; Pval = ", cor_results$p.value))
#' 
#' 
#' 
#' #----------------------------
#' # Contrast two models
#' #----------------------------
#' 
#' # Load zero shot model scores
#' model_data <- readRDS("~/ProteinGym_data/EH_data/v1.2/ProGym217_zeroshot_scores_v1.2.rds")
#' 
#' assay_name <- "A0A140D2T1_ZIKV_Sourisseau_2019"
#' model1 <- "EVE_ensemble"
#' model2 <- "S3F_MSA"
#' 
#' # Filter selected assay
#' model_df <- model_data[[assay_name]]
#' 
#' ## Filter out multiple aa sites
#' model_df <- model_df |>  
#'     filter(!grepl(":", .data$mutant))
#' 
#' ## Select chosen model
#' model_df <- model_df |>
#'     dplyr::select(
#'         mutant,
#'         all_of(c(model1 = model1, model2 = model2))
#'     )
#' 
#' ## Corr function
#' pg_correlate <- 
#'     function(model_df)
#' {
#'     cor_results <- 
#'         cor.test(
#'             model_df$model1, model_df$model2, 
#'             method=c("spearman"), 
#'             exact = FALSE
#'         )
#'     
#'     cor_results
#' }
#' 
#' cor_results <- pg_correlate(model_df = model_df)
#' 
#' ## Correlation density plot
#' pg_density_plot <- 
#'     model_df |> 
#'     ggplot(
#'         aes(x = .data$model1, y = .data$model2)
#'     ) +
#'     geom_bin2d(bins = 60) +
#'     scale_fill_continuous(type = "viridis") +
#'     xlab(paste0(model1, "\nzero shot score")) +
#'     ylab(paste0(model2, "\nzero shot score")) +
#'     theme_classic() +
#'     theme(
#'         axis.text.x = element_text(size = 16),
#'         axis.text.y = element_text(size = 16),
#'         axis.title.y = element_text(size = 16, vjust = 2),
#'         axis.title.x = element_text(size = 16, vjust = 0),
#'         legend.title = element_text(size = 16),
#'         legend.text = element_text(size = 16)
#'     )
#' 
#' pg_density_plot
#' 
#' print(paste0("r = ", format(round(cor_results$estimate, 2)), 
#'                 "; Pval = ", cor_results$p.value))
