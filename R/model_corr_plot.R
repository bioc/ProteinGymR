#' @rdname model_corr_plot
#'
#' @noRd
#' 
#' @importFrom dplyr filter pull as_tibble rename_with mutate case_when
#' @importFrom queryup query_uniprot
#'
pg_filter_am_table <-
    function(am_table, uID)
{
    ## Check if am_table is missing
    if (missing(am_table)) {
     
        ## Load default AlphaMissense data
        am_table <- am_scores()
        
        ## Rename columns to match default model_tables
        new_cols <- c('UniProt_id', 'mutant')

        am_table <- 
            am_table |> 
            rename_with(
                ~ new_cols, 
                .cols = c('Uniprot_ID', 'variant_id')
            )
        
        ## Default am_table IDs are in SwissProt. Convert to UniProt
        query <- list("accession_id" = uID)
        res <- query_uniprot(query = query, show_progress = TRUE)
        swissID <- res |> pull(.data$`Entry Name`)
        
        ## Check that UniProt/SwissID valid
        if (NROW(res) != 1){
            stop("UniProt: '", uID, "' is not valid; ",
            "check that the UniProt ID is correct")
        }
       
        ## Replace swissID observations with uID
        am_table <-
            am_table |>
            mutate(
                UniProt_id = case_when(
                    (.data$UniProt_id) == swissID ~ uID,
                    TRUE ~ as.character(.data$UniProt_id)
                )
            )
        am_table
    }
        
    ## Filter for uID
    alphamissense_table <-
        am_table |>
        filter(.data$UniProt_id == uID) |> 
        as_tibble()

    ## Check if table is empty after filtering
    if (!NROW(alphamissense_table)) {
        stop(
            "no AlphaMissense information found for the protein ",
            "accession '", uID, "'; check that the UniProt ID is correct"
        )
    }
    alphamissense_table
}

#' @rdname model_corr_plot
#'
#' @noRd
#' 
#' @importFrom dplyr as_tibble bind_rows
#'
pg_filter_model_table <-
    function(model_table, uID)
{
    ## Extract assays containg uID
    model_list <- Filter(function(df) {
        !is.null(df) && "UniProt_id" %in% colnames(df) && 
            any(df$UniProt_id == uID)}, model_table)
    
    ## Error if no assay found for uID
    if (length(model_list) == 0) {
        stop("no ProteinGym assay found for the protein ",
            "accession '", uID, "'; check that the UniProt ID is correct")
    }
    
    ## Combine into one dataframe
    model_table <-
        model_list |>
        bind_rows() |>
        as_tibble()
    
    return(model_table)
}

# Assign source dataframe based on model specified
#' @rdname model_corr_plot
#' 
#' @noRd
#' 
#' @importFrom dplyr select all_of
get_model_df <- function(model, uniprotId) {
    if (model == "AlphaMissense") {
        model_table <- am_scores()
        model_table <- pg_filter_am_table(uID = uniprotId)
        
    } else if (model %in% available_models()) {
        model_table <- zeroshot_substitutions()
        model_table <- pg_filter_model_table(
            model_table = model_table,
            uID = uniprotId)

        model_table <- model_table |> 
            select(all_of(c("DMS_id", "UniProt_id", "mutant", model)))
        
    } else if (model %in% supervised_available_models()) {
        model_table <- supervised_substitutions()
        model_table <- pg_filter_model_table(
            model_table = model_table,
            uID = uniprotId)
    
        model_table <- model_table |> 
            select(all_of(c("DMS_id", "UniProt_id", "mutant", model)))
        
    } else {
        stop(paste("Model", model, "not recognized."))
    }
    return(model_table)
}

#' Merge model and dms tables by UniProt and mutant IDs
#'
#' @noRd
#'
#' @importFrom dplyr left_join select group_by summarise
#' @importFrom stats na.omit
#' @importFrom rlang sym
#' 
model_match_id <- 
    function(modeldf1 = model_df1, modeldf2 = model_df2,
        model1, model2)
{
    ## Check that UniProt IDs are the same across tables
    stopifnot(
        unique(modeldf1$UniProt_id) == unique(modeldf2$UniProt_id)
    )
    
    ## Merge tables and select relevant columns  
    merged_table <- 
        left_join(
            modeldf1, modeldf2, 
            by = c("UniProt_id", "mutant"),
            relationship = "many-to-many"
        )
    
    ## Only keep one DMS_id column
    if (all(c("DMS_id.x", "DMS_id.y") %in% colnames(merged_table))) {
        merged_table <- merged_table |>
            mutate(DMS_id = DMS_id.y) |>
            select(-DMS_id.x, -DMS_id.y)
    }

    cols <- c("DMS_id", "UniProt_id", "mutant", model1, model2)
    merged_table <- merged_table |>
        select(all_of(cols)) |>
        na.omit()

    ## Average model scores across multiple studies per protein
    model_sym1 <- rlang::sym(model1)
    model_sym2 <- rlang::sym(model2)
    
    merged_table <- 
        merged_table |>
        group_by(UniProt_id, mutant) |>
            summarise(
                mean_model1 = mean(!!model_sym1, na.rm = TRUE),
                mean_model2 = mean(!!model_sym2, na.rm = TRUE),
                .groups = "drop"
            )
    merged_table
}

#' Average Spearman correlation per protein
#'
#' @noRd
#' @importFrom stats cor.test
#'
pg_model_corr <- 
    function(merged_table)
{
    cor_results <- 
        cor.test(
            merged_table$mean_model1, merged_table$mean_model2, 
            method=c("spearman"), 
            exact = FALSE
        )
    cor_results
}

#' @rdname model_corr_plot
#' 
#' @title Compare Prediction Scores for ProteinGym Models
#' 
#' @description `model_corr_plot()` runs a Spearman correlation 
#'    between predicted model scores for two models in ProteinGym v1.2. 
#'    Returns a ggplot object for visualization.
#'
#' @param uniprotId `character()` a valid UniProt accession identifier.
#' 
#' @param model1 `character()` first model to plot. 
#' 
#' @param model2 `character()` second model to plot.
#'
#' @details
#'
#' For `model_corr_plot()`:
#' 
#' `model1` and `model2` must be valid models.
#'    To view the possible zero-shot and semi-supervised models available in 
#'    ProteinGym v1.2 run `ProteinGymR::available_models()` or 
#'    `ProteinGymR::supervised_available_models()`, respectively, or set the 
#'    model to 'AlphaMissense' for AlphaMissense predictions.
#'    If no models are specified, the default loads in AlphaMissense and GEMME.
#'    
#' @return `model_corr_plot()` returns a `ggplot` object visualizing 
#'    the Spearman correlation between the predicted scores generated by two 
#'    models in ProteinGym and prints the p-value of the analysis to the console.
#'
#' @examples
#' 
#' # Use defaults. Only requires uniprotId
#' model_corr_plot(uniprotId = "Q9NV35")
#' 
#' model_corr_plot(
#'     uniprotId = "P04637",
#'     model1 = "Kermut",
#'     model2 = "EVE_single"
#' )
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
#
#' @importFrom dplyr filter pull as_tibble rename_with mutate case_when
#'
#' @importFrom ExperimentHub ExperimentHub
#' 
#' @export
model_corr_plot <-
    function(
        uniprotId, 
        model1 = "AlphaMissense",
        model2 = "GEMME"
    )
{
    ## Check function dependencies
    if (!requireNamespace("ggExtra", quietly = TRUE))
        stop(paste("Required package \'ggExtra\' not found.", 
                    "Use \'BiocManager::install(\"ggExtra\") to install it."))
        
    if (!requireNamespace("ggplot2", quietly = TRUE))
        stop(paste("Required package \'ggplot2\' not found.", 
                    "Use \'BiocManager::install(\"ggplot2\") to install it."))
        
    ## Validate required uniprotId argument
    stopifnot(is.character(uniprotId))
    
    ## Validate user-specified models
    valid_models <- c(
        available_models(), 
        supervised_available_models(),
        "AlphaMissense") 
    
    if (!all(model1 %in% valid_models)) {
        invalid_models <- model1[!model1 %in% valid_models]
        stop(paste("Invalid model 1 specified:", invalid_models))
    }
    
    if (!all(model2 %in% valid_models)) {
        invalid_models <- model2[!model2 %in% valid_models]
        stop(paste("Invalid model 2 specified:", invalid_models))
    }

    ## Load respective data for uniprotId
    model_df1 <- get_model_df(model1, uniprotId = uniprotId)
    model_df2 <- get_model_df(model2, uniprotId = uniprotId)

    ## Join tables by uniprotId
    merged_table <-
        model_match_id(
            modeldf1 = model_df1, 
            modeldf2 = model_df2,
            model1 = model1,
            model2 = model2
        )

     ## Check if merged table is empty
    if (!NROW(merged_table)) {
        stop(
            "No common mutants between chosen models for ",
            "accession '", uID, "'"
        )
    }
    
    cor_results <- pg_model_corr(merged_table)
    
    ## Correlation density plot
    pg_density_plot <- 
        merged_table |> 
        ggplot2::ggplot(
            ggplot2::aes(x = .data$mean_model1, y = .data$mean_model2)
        ) +
        ggplot2::geom_bin2d(bins = 60) +
        ggplot2::geom_point(alpha = 0) +
        ggplot2::scale_fill_continuous(type = "viridis") +
        ggplot2::labs(title = paste0("\nUniProt ID: ", uniprotId)) +
        ggplot2::xlab(paste(model1, "score")) +
        ggplot2::ylab(paste(model2, "score")) +
        ggplot2::theme_classic() +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(size = 16),
            axis.text.y = ggplot2::element_text(size = 16),
            axis.title.y = ggplot2::element_text(size = 16, vjust = 2),
            axis.title.x = ggplot2::element_text(size = 16, vjust = 0),
            legend.title = ggplot2::element_text(size = 16),
            legend.text = ggplot2::element_text(size = 16)
        )
    
    # Add marginal density plots
    pg_density_plot <- ggExtra::ggMarginal(
        pg_density_plot,
        type = "densigram", # Can also use "histogram"
        fill = "#B0C4DE", 
        color = "black"  # Change color as needed
    )
    print(paste0("r = ", format(round(cor_results$estimate, 2)), 
                "; Pval = ", cor_results$p.value))
    pg_density_plot
}
