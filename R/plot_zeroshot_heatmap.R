#' @rdname plot_zeroshot_heatmap
#'
#' @noRd
#' 
#' @importFrom dplyr filter
#'

filter_by_pos <- 
    function(df, start_pos = NULL, end_pos = NULL)
{
    ## Check pos column
    if (!"pos" %in% colnames(df)) {
        stop("The dataframe must contain a 'pos' column.")
    }
        
    if (!is.integer(df$pos)) {
        stop("The 'pos' column must be an integer vector.")
    }
    
    ## Grab minimum and maximum values of the pos column
    min_pos <- min(df$pos, na.rm = TRUE)
    max_pos <- max(df$pos, na.rm = TRUE)
  
    ## Check if user-provided start_pos or end_pos is within the range
    if (!is.null(start_pos) && (start_pos > max_pos)) {
        stop(sprintf("start_pos (%d) is outside the assay range (%d to %d)", 
            start_pos, min_pos, max_pos))
    }
    if (!is.null(end_pos) && (end_pos < min_pos)) {
        stop(sprintf("end_pos (%d) is outside the assay range (%d to %d)", 
            end_pos, min_pos, max_pos))
    }
    
    ## If start or end is NULL, default to min or max "pos"
    if (is.null(start_pos)) start_pos <- min_pos
    if (is.null(end_pos)) end_pos <- max_pos
        
    ## Filter the dataframe based on the specified positions
    filtered_df <- df |> 
        filter(pos >= start_pos & pos <= end_pos)
  
    return(filtered_df)
}

#' @rdname plot_zeroshot_heatmap
#'
#' @noRd
#'

filter_exact_coord <- 
    function(assay_pos, start_pos = NULL, end_pos = NULL, exact_coord = NULL)
{
    if (missing(exact_coord)) {
     
        message(paste(
            "'exact_coord' not provided,",
            "using only positions available in assay."
        ))
     
        assay_pos
     
    } else if (exact_coord == FALSE) {
    
        assay_pos
    
    } else if (exact_coord == TRUE) {
        
        if (is.null(start_pos)) start_pos <- min(assay_pos$pos)
        if (is.null(end_pos)) end_pos <- max(assay_pos$pos)
    
        ## Create a sequence of consecutive positions
        all_pos <- seq(start_pos, end_pos)
        
        ## Merge with full sequence and fill missing values with NA
        assay_pos <- merge(
            data.frame(pos = all_pos),
            assay_pos,
            by = "pos",
            all.x = TRUE
        )
        
        assay_pos
        
    } else {
        
        assay_pos
    }
    }

#' @rdname plot_zeroshot_heatmap
#' 
#' Create a color function for the heatmap
#' 
#' @noRd
make_col_fun_model <- function(mat, color_scheme = "default") {
    min_val <- min(mat, na.rm = TRUE)
    max_val <- max(mat, na.rm = TRUE)
    mid_val <- max_val/2

    if (color_scheme == "EVE") {
        mid_val <- max_val/2
        halfpt <- min_val / 2
        col_fun <- colorRamp2::colorRamp2(
            c(min_val, halfpt, mid_val, max_val),
            c("#000", "#9440e8", "#00CED1", "#fde662")
        )
    } else {
        parula_colors <- pals::parula(3)
        col_fun <- colorRamp2::colorRamp2(
            c(min_val, mid_val, max_val),
            parula_colors
        )
    }
    return(col_fun)
}


#' @rdname plot_zeroshot_heatmap
#' 
#' @title Visualize Zero Shot Scores Along a Protein
#' 
#' @description `plot_zeroshot_heatmap()` plots predicted model scores under the 
#'    zero-shot model for amino acid substitutions along a protein in a defined 
#'    DMS assay. 
#'
#' @param assay_name `character()` a valid assay name. For the full list of 
#'    available assays, run `names()` on the list object loaded with 
#'    `ProteinGymR::zeroshot_substitutions()`. Alternatively, the name of a 
#'    user-defined DMS assay.
#' 
#' @param model_data `list()` object of zero-shot assays loaded with 
#'   `ProteinGymR::zeroshot_substitutions()`.
#'    Alternatively, a user-defined list of assays with names corresponding
#'    to `assay_name` param.
#'    
#' @param model `character()` one of the 79 zero-shot models to plot. To view
#'    the list of models, runs `ProteinGymR::available_models()`.
#'
#' @param start_pos `integer()` first amino acid position to plot. If missing, 
#'    default start is at the first position along the protein where zero shot 
#'    scores are available. 
#'    
#' @param end_pos `integer()` last amino acid position to plot. If missing, 
#'    default end is at the last position along the protein where zero shot 
#'    scores are available. 
#'    
#' @param exact_coord `logical()` TRUE will plot the precise `start_pos` 
#'    and `end_pos` coordinates defined. By default, `exact_coord` is set to 
#'    FALSE, plotting only amino acid positions with available data in the 
#'    chosen assay.
#'    
#' @param cluster_rows `logical()` defaults to FALSE. See argument details in 
#'    [ComplexHeatmap::Heatmap].
#' 
#' @param cluster_columns `logical()` defaults to FALSE. See argument details in 
#'    [ComplexHeatmap::Heatmap].
#'    
#' @param color_scheme `character()` defaults to blue, white, and red to 
#'    represent positive, neutral, negative scores. Set argument equal to "EVE" 
#'    to use the color scheme consistent with the popEVE portal.
#'    
#' @param ... additional arguments passed to internal plotting functions.
#' 
#' @details
#'
#' For `plot_zeroshot_heatmap()`, 
#'    `model_data` must be a `list()` object with set names for each assay 
#'    element matching `assay_name` parameter.
#'    
#' Each assay in the `model_data()` must include the following columns:
#'
#' - `mutant`: Mutant identifier string matching.
#'    Specifically, the set of substitutions to apply on the reference sequence 
#'    to obtain the mutated sequence (e.g., A1P:D2N implies the amino acid 'A' 
#'    at position 1 should be replaced by 'P', and 'D' at position 2 should be 
#'    replaced by 'N').
#' -  A column with predicted model scores for each substitution. Column name 
#'    should match `model` argument string.
#'
#' @return Returns a [ComplexHeatmap::Heatmap] plot of zero-shot model scores 
#'    for each position along a protein in a chosen DMS substitution assay. 
#'    The x-axis shows amino acid positions where a DMS mutation exist, and the 
#'    y-axis represents possible amino acid residues, ordered by default based 
#'    on the physiochemical groupings. Higher and lower DMS scores indicate a 
#'    more positive or negative fitness effect after the mutation, respectively.
#'
#' @examples
#' 
#' available_models()
#' 
#' plot_zeroshot_heatmap(assay_name = "A0A192B1T2_9HIV1_Haddox_2018", 
#'     model = "GEMME",
#'     start_pos = 600,
#'     end_pos = 700, 
#'     color_scheme = "EVE")
#'     
#' plot_zeroshot_heatmap(assay_name = "SRC_HUMAN_Nguyen_2022",
#'     model = "CARP_38M")
#' 
#' @importFrom dplyr filter pull as_tibble rename_with mutate 
#'              arrange select
#'              
#' @importFrom tidyr pivot_wider
#' 
#' @importFrom stringr str_sub
#'
#' @export
plot_zeroshot_heatmap <- 
    function(
        assay_name, 
        model_data, # Give it the zero-shot model list
        model = NULL, # Which model to select?
        start_pos = NULL, 
        end_pos = NULL, 
        exact_coord = FALSE,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        color_scheme,
        ...) 
{
        
        ## Check dependencies
        if (!requireNamespace("circlize", quietly = TRUE))
        stop(paste("Required package \'circlize\' not found.", 
                    "Use \'BiocManager::install(\"circlize\") to install it."))
        
        if (!requireNamespace("ComplexHeatmap", quietly = TRUE))
            stop(paste("Required package \'ComplexHeatmap\' not found.", 
                "Use \'BiocManager::install(\"ComplexHeatmap\") to install it."))
    
        if (!requireNamespace("grid", quietly = TRUE))
            stop(paste("Required package \'grid\' not found.", 
                "Use \'BiocManager::install(\"grid\") to install it."))
        
        if (!requireNamespace("pals", quietly = TRUE))
            stop(paste("Required package \'pals\' not found.", 
                "Use \'BiocManager::install(\"pals\") to install it."))
        
        ## Check model is defined
        if (missing(model)){
            stop("Please define a `model` to explore. See all options ", 
                "with: `available_models()`."
                )
        }
        
        ## If model_data argument missing
        if (missing(model_data)) {
     
            message(paste(
                "'model_data' not provided,",
                "using default data loaded with zeroshot_substitutions()"
            ))
         
            model_data <- zeroshot_substitutions()
     
        } else {
            model_data
        }
        
        ## Extract the specified assay
        assay_df <- model_data[[assay_name]]
    
        ## Filter out multiple aa sites
        assay_df <- assay_df |>  
            filter(!grepl(":", .data$mutant))
        
        ## Stop if all rows are multiple sites
        if (nrow(assay_df) == 0){
                stop("Unable to plot zero shot substitution heatmap; ", 
                    "assay: '", assay_name, "' contains only ", 
                    "multiple amino acid sites."
                )
        }
        
        ## Select chosen model
        assay_df <- assay_df |>
            dplyr::select(
                .data$mutant,
                tidyselect::all_of(model)
            )
        
        ## Wrangle the data
        assay_df <- assay_df |>
            mutate(
            ref = str_sub(.data$mutant, 1, 1),
            pos = as.integer(
                    gsub(".*?([0-9]+).*", "\\1", .data$mutant)
                    ), 
            alt = str_sub(.data$mutant, -1)
            )
        
        ## Reshape to wide format
        assay_wide <- assay_df |>
            dplyr::select(-.data$mutant) |>
            pivot_wider(names_from = .data$alt, values_from = model) |>
            arrange(.data$pos)
    
        ## Subset to start_pos and end_pos, or default to first and last sites.
        if (is.null(start_pos)) {
            message(paste(
                "'start_pos' not provided,",
                "using the first position in the protein."
            ))
        }
        
        if (is.null(end_pos)) {
            message(paste(
                "'end_pos' not provided,",
                "using the last position in the protein."
            ))
        }
        
        assay_pos <- filter_by_pos(
            df = assay_wide, 
            start_pos = start_pos, 
            end_pos = end_pos
            )
        
        ref_df <- filter_by_pos(
            df = assay_df,
            start_pos = start_pos, 
            end_pos = end_pos)
        
        ## exact_coord
        assay_pos <- filter_exact_coord(
            assay_pos, 
            start_pos = start_pos, 
            end_pos = end_pos,
            exact_coord = exact_coord
        )
        
        ## Define a text annotation for the columns
        column_annotation <- assay_pos |> 
            dplyr::select(.data$ref, .data$pos) |> 
            unique()
        
        ## cluster_columns with NA check
        if (sum(is.na(column_annotation)) > 0 & cluster_columns == TRUE){
                stop("Protein range includes missing values, preventing ", 
                    "clustering of columns. Try setting exact_coord argument ",
                    "to FALSE."
                )
        }
        
        
        column_annotation[is.na(column_annotation)] <- " "
    
        ## Define a text annotation for the columns
        column_annotation <- ComplexHeatmap::columnAnnotation(
          text = ComplexHeatmap::anno_text(column_annotation$ref, 
               rot = 0, just = "right", gp = grid::gpar(fontsize = 10))
        )
        
        ## Convert to matrix
        pos <- assay_pos$pos
        alt <- colnames(assay_pos)
        alt <- alt[-c(1)]
        
        assay_pos <- assay_pos |>
            dplyr::select(-c(.data$ref))
        
        heatmap_matrix <- assay_pos |>
            dplyr::select(2:length(assay_pos)) |> as.matrix()
        
        ## Set aa pos as rownames of matrix and transpose
        rownames(heatmap_matrix) <- pos
        heatmap_matrix <- t(heatmap_matrix)
        
        ## Reorder rows based on physiochemical properties
        phyiochem_order <- "DEKRHNQSTPGAVILMCFYW"
        phyiochem_order <- unlist(strsplit(phyiochem_order, split = ""))
        
        reordered_matrix <- heatmap_matrix[match(phyiochem_order, 
                                rownames(heatmap_matrix)), ]
        
        ## Create the heatmap
        if (missing(color_scheme)) {
            color_scheme <- "default"
        }
        col_fun <- make_col_fun_model(reordered_matrix, color_scheme)
        
        ComplexHeatmap::Heatmap(reordered_matrix,
            name = paste(model),
            heatmap_legend_param = list(
                at = c(min(reordered_matrix,na.rm = TRUE), 
                       ((max(reordered_matrix, na.rm = TRUE) - 
                            min(reordered_matrix, na.rm = TRUE))/2),
                       max(reordered_matrix, na.rm = TRUE)
                       ),
                labels = c(
                    sprintf("%.2f", min(reordered_matrix, na.rm = TRUE)),
                    sprintf("%.2f",(
                        (max(reordered_matrix, na.rm = TRUE) - 
                            min(reordered_matrix, na.rm = TRUE))/2)
                    ),
                    sprintf("%.2f", max(reordered_matrix, na.rm = TRUE))
                )
            ),
            cluster_rows = cluster_rows,
            cluster_columns = cluster_columns,
            col = col_fun,
            na_col = "grey",
            top_annotation = column_annotation,
            ...)
}