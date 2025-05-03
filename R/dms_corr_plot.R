#' @rdname dms_corr_plot
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
        
        ## Rename columns to match default dms_table
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

#' Filter the ProteinGym model table for uID
#' @rdname dms_corr_plot
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
        stop("no ProteinGym DMS assay found for the protein ",
            "accession '", uID, "'; check that the UniProt ID is correct")
    }
    
    ## Combine into one dataframe
    model_table <-
        model_list |>
        bind_rows() |>
        as_tibble()
    
    return(model_table)
}
        
#' Filter the ProteinGym DMS table with uID
#'
#' @noRd
#' 
#' @importFrom dplyr as_tibble bind_rows
#' @importFrom purrr keep
#'
pg_filter_dms_table <-
    function(pg_table, uID)
{
    ## Check if pg_table is missing
    if (missing(pg_table)) {
        message(paste(
            "'dms_table' not provided, using default table from",
            "`ProteinGymR::dms_substitutions()`"
        ))
        pg_table <- dms_substitutions()
    }
    
    ## Filter pg_table for uID, rbind into one data.frame. Remove NA UniProts
    pg_table <- Filter(function(df) !any(is.na(df[["UniProt_id"]])), pg_table)
    
    filtered_pg <- purrr::keep(pg_table, ~ any(.x$UniProt_id == uID))
    
    dms_table <-
        filtered_pg |>
        bind_rows() |>
        as_tibble()
    
    ## Check if table is empty after filtering
    if (!NROW(dms_table)) {
        stop(
            "no DMS substitution information found for the protein ",
            "accession '", uID, "'"
        )
    }
    dms_table
}

#' Merge model and dms tables by UniProt and mutant IDs
#'
#' @noRd
#'
#' @importFrom dplyr left_join select group_by summarise
#' @importFrom stats na.omit
#' @importFrom rlang sym
#' 
pg_match_id <- 
    function(model_table, pg_table, model)
{
    ## Check that UniProt IDs are the same across tables
    stopifnot(
        unique(model_table$UniProt_id) == unique(pg_table$UniProt_id)
    )
    
    ## Merge tables and select relevant columns  
    merged_table <- 
    left_join(
        model_table, pg_table, 
        by = c("UniProt_id", "mutant"),
        relationship = "many-to-many"
    )

    ## Only keep one DMS_score column
    if (all(c("DMS_score.x", "DMS_score.y") %in% colnames(merged_table))) {
        merged_table <- merged_table |>
            mutate(DMS_score = DMS_score.y) |>
            select(-DMS_score.x, -DMS_score.y)
    }

    cols <- c("UniProt_id", "mutant", model, "DMS_score")
    merged_table <- merged_table |>
        select(all_of(cols)) |>
        na.omit()

    ## Average model and dms scores across multiple studies per protein
    model_sym <- rlang::sym(model)  # turn model string into a symbol

    merged_table <- 
        merged_table |>
        group_by(UniProt_id, mutant) |>
            summarise(
                mean_model = mean(!!model_sym, na.rm = TRUE),
                mean_dms   = mean(DMS_score, na.rm = TRUE),
                .groups = "drop"
            )
    merged_table
}

#' Average Spearman correlation per protein
#'
#' @noRd
#' @importFrom stats cor.test
#'
pg_correlate <- 
    function(merged_table)
{
    cor_results <- 
        cor.test(
            merged_table$mean_model, merged_table$mean_dms, 
            method=c("spearman"), 
            exact = FALSE
        )
    cor_results
}

#' @rdname dms_corr_plot
#' 
#' @title Integrate ProteinGym DMS and Model Prediction Scores
#' 
#' @description `dms_corr_plot()` runs a Spearman correlation 
#'    between ProteinGym deep mutational scanning (DMS) assay scores and 
#'    predicted model scores. 
#'    Returns a ggplot object for visualization.
#'
#' @param uniprotId `character()` a valid UniProt accession identifier.
#' 
#' @param model `character()` a model to plot. To view the possible zero-shot 
#'    and semi-supervised models available in ProteinGym v1.2 run 
#'    `ProteinGymR::available_models()` or 
#'    `ProteinGymR::supervised_available_models()`, respectively, or set 
#'    `model = "AlphaMissense` to access AlphaMissense predictions.
#'    If no `model` argument is specified, the default loads in the 
#'    supplemental table from the AlphaMissense paper.
#'
#' @param dms_table a table containing deep mutational scanning (DMS) 
#'    assay scores for mutations. The default table loads substitutions from 
#'    [ProteinGym](https://proteingym.org/download).
#'    Alternatively, a user-defined [`tibble::tbl_df`] or [`data.frame`]
#'    can be supplied.
#'
#' @details
#'
#' For `dms_corr_plot()`, 
#'    `model_table` columns must include:
#'
#' - `UniProt_id`: UniProt accession identifier.
#' - `mutant`: Mutant identifier string matching the `dms_table` format. 
#'    Protein position in the middle, and the reference and mutant 
#'    amino acid residues to the left and right of the position, respectively.
#' - `{{model}}`: Predicted model scores. Set this column name as the model 
#'    name.
#'
#' `dms_table` columns must include:
#'
#' - `UniProt_id`: UniProt accession identifier.
#' - `mutant`: Mutant identifier string matching `model_table` variants. 
#'    Specifically, the set of substitutions to apply on the reference sequence 
#'    to obtain the mutated sequence (e.g., A1P:D2N implies the amino acid 'A' 
#'    at position 1 should be replaced by 'P', and 'D' at position 2 should be 
#'    replaced by 'N').
#' - `DMS_score`: Experimental measurement in the DMS assay. 
#'    Higher values indicate higher fitness of the mutated protein.
#'    
#' @return `dms_corr_plot()` returns a `ggplot` object visualizing 
#'    the Spearman correlation between experimental DMS scores and predicted
#'    model scores and prints the r and p-value of the analysis to console.
#'
#' @examples
#' 
#' # Use defaults. Only requires uniprotId
#' dms_corr_plot(uniprotId = "Q9NV35")
#' 
#' dms_corr_plot(
#'     uniprotId = "P04637",
#'     model = "Kermut"
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
#' 
#' @importFrom ggplot2 ggplot geom_bin2d aes element_text labs xlab ylab
#'     scale_fill_continuous theme_classic annotate theme geom_point
#' @importFrom ggExtra ggMarginal
#' @importFrom lifecycle is_present deprecate_stop deprecated
#' 
#' @export
dms_corr_plot <-
    function(
        uniprotId, 
        alphamissense_table = deprecated(), 
        dms_table, 
        model = "AlphaMissense"
    )
{
    ## Deprecate alphamissense_table argument, replace with model_table
    if (lifecycle::is_present(alphamissense_table)) {
        lifecycle::deprecate_stop(
            "1.2.0",
            what = "ProteinGymR::dms_corr_plot(alphamissense_table = )",
            with = 'ProteinGymR::dms_corr_plot(model = "AlphaMissense")'
        )
    }

    ## Validate required uniprotId argument
    stopifnot(is.character(uniprotId))
    
    ## Validate user-specified model
    valid_models <- c(
        available_models(), 
        supervised_available_models(),
        "AlphaMissense") 
    
    if (!all(model %in% valid_models)) {
        invalid_models <- model[!model %in% valid_models]
        stop(paste("Invalid model(s) specified:", invalid_models))
    }

    ## Use AlphaMissense as default model if model missing
    if (model == "AlphaMissense") {
        message("Using default AlphaMissense model", 
            "from `ProteinGymR::am_scores()`.")
        model_table <- pg_filter_am_table(uID = uniprotId)
    
    } else if (model %in% available_models()) {
        model_table <- zeroshot_substitutions()
        model_table <- pg_filter_model_table(
            model_table = model_table,
            uID = uniprotId)
    
    } else if (model %in% supervised_available_models()) {
        model_table <- supervised_substitutions()
        model_table <- pg_filter_model_table(
            model_table = model_table,
            uID = uniprotId)
    
    } else {
        stop("Please select a valid model to evaluate.")
    }

    ## Filter DMS assay to selected uID
    dms_table <-
        pg_filter_dms_table(
            pg_table = dms_table,
            uID = uniprotId
        )
    
    ## Join tables by uniprotId
    merged_table <-
        pg_match_id(model_table = model_table, 
            pg_table = dms_table,
            model = model)

     ## Check if merged table is empty
    if (!NROW(merged_table)) {
        stop(
            "no common mutants between chosen model and DMS scores for ",
            "accession '", uID, "'"
        )
    }
    
    cor_results <- pg_correlate(merged_table)
    
    ## Correlation density plot
    pg_density_plot <- 
        merged_table |> 
        ggplot(
            aes(y = .data$mean_model, x = .data$mean_dms)
        ) +
        geom_bin2d(bins = 60) +
        geom_point(alpha = 0) +
        scale_fill_continuous(type = "viridis") +
        labs(title = paste0("\nUniProt ID: ", uniprotId)) +
        xlab("DMS score") +
        ylab(paste(model, "score")) +
        theme_classic() +
        theme(
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title.y = element_text(size = 16, vjust = 2),
            axis.title.x = element_text(size = 16, vjust = 0),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 16)
        )
    
    # Add marginal density plots
    pg_density_plot <- ggMarginal(
        pg_density_plot,
        type = "densigram", # Can also use "histogram"
        fill = "#B0C4DE", 
        color = "black"  # Change color as needed
    )
    print(paste0("r = ", format(round(cor_results$estimate, 2)), 
                "; Pval = ", cor_results$p.value))
    pg_density_plot
}