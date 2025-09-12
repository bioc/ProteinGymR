#' @rdname plot_dms_heatmap
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

#' @rdname plot_dms_heatmap
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

#' Create a color function for the heatmap
#' 
#' @noRd
#' 
make_col_fun_dms <- function(mat, color_scheme = "default") {
    
    if (!requireNamespace("colorRamp2", quietly = TRUE))
        stop(paste("Required package \'colorRamp2\' not found.", 
                    "Use \'BiocManager::install(\"colorRamp2\") to install it."))
    
    if (color_scheme == "EVE") {
        halfpt <- (min(mat, na.rm = TRUE) / 2)
        col_fun <- colorRamp2::colorRamp2(
            c(min(mat, na.rm = TRUE), halfpt, 0, max(mat, na.rm = TRUE)),
            c("#000", "#9440e8", "#00CED1", "#fde662")
        )
    } else {
        col_fun <- colorRamp2::colorRamp2(
            c(min(mat, na.rm = TRUE), 0, max(mat, na.rm = TRUE)),
            c("red", "white", "blue")
        )
    }
    return(col_fun)
}

#' @rdname plot_dms_heatmap
#' 
#' @title Visualize DMS Scores Along a Protein
#' 
#' @description `plot_dms_heatmap()` plots DMS scores for amino acid
#'    substitutions along a protein in a defined DMS assay. 
#'
#' @param assay_name `character()` a valid DMS assay name. For the full list of 
#'    available assays, run `names()` on the list object loaded with 
#'    `ProteinGymR::dms_substitutions()`. Alternatively, the name of a 
#'    user-defined DMS assay.
#' 
#' @param dms_data `list()` object of DMS assays loaded with 
#'   `ProteinGymR::dms_substitutions()` if not specified by user.
#'    Alternatively, a user-defined list of DMS assays with names corresponding
#'    to `assay_name` param.
#'
#' @param start_pos `integer()` first amino acid position to plot. If missing, 
#'    default start is at the first position along the protein where DMS scores 
#'    are available. 
#'    
#' @param end_pos `integer()` last amino acid position to plot. If missing, 
#'    default end is at the last position along the protein where DMS scores 
#'    are available. 
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
#'  represent positive, neutral, negative scores. Set argument equal to "EVE" 
#'  to use the color scheme consistent with the popEVE portal.
#'  
#' @param ... additional arguments passed to internal plotting functions.
#' 
#' @details
#'
#' For `plot_dms_heatmap()`, 
#'    `dms_data` must be a `list()` object with set names for each assay 
#'    element matching `assay_name` parameter.
#'    
#' Each assay in the `dms_data()` must include the following columns:
#'
#' - `mutant`: Mutant identifier string matching.
#'    Specifically, the set of substitutions to apply on the reference sequence 
#'    to obtain the mutated sequence (e.g., A1P:D2N implies the amino acid 'A' 
#'    at position 1 should be replaced by 'P', and 'D' at position 2 should be 
#'    replaced by 'N').
#' - `DMS_score`: Experimental measurement in the DMS assay. 
#'    Higher values indicate higher fitness of the mutated protein.
#'
#' @return Returns a [ComplexHeatmap::Heatmap] plot of DMS scores for each 
#'    position along a protein in a chosen DMS assay. The x-axis shows amino 
#'    acid positions where a DMS mutation exist, and the y-axis represents 
#'    possible amino acid residues, ordered by default based on the 
#'    physiochemical groupings. Higher and lower DMS scores indicate a more 
#'    positive or negative fitness effect after the mutation, respectively.
#'
#' @examples
#' 
#' dms_data <- dms_substitutions()
#' 
#' plot_dms_heatmap(assay_name = "A0A192B1T2_9HIV1_Haddox_2018", 
#'     start_pos = 10, 
#'     end_pos = 80)
#'     
#' plot_dms_heatmap(assay_name = "A0A192B1T2_9HIV1_Haddox_2018", 
#'     start_pos = 10, 
#'     end_pos = 80, 
#'     exact_coord = TRUE,
#'     color_scheme = "EVE")
#'     
#' plot_dms_heatmap(assay_name = "A0A192B1T2_9HIV1_Haddox_2018", 
#'     start_pos = 50, 
#'     end_pos = 100, 
#'     cluster_rows = TRUE)
#' 
#' @importFrom dplyr filter pull as_tibble rename_with mutate 
#'              arrange select
#'              
#' @importFrom tidyr pivot_wider
#' 
#' @importFrom stringr str_sub
#'
#' @export
plot_dms_heatmap <- 
    function(
        assay_name, 
        dms_data, 
        start_pos = NULL, 
        end_pos = NULL, 
        exact_coord = FALSE,
        cluster_rows = FALSE,
        cluster_columns = FALSE,
        color_scheme,
        ...) 
{

    ## Check function dependencies
    if (!requireNamespace("circlize", quietly = TRUE))
        stop(paste("Required package \'circlize\' not found.", 
                    "Use \'BiocManager::install(\"circlize\") to install it."))
        
    if (!requireNamespace("ComplexHeatmap", quietly = TRUE))
        stop(paste("Required package \'ComplexHeatmap\' not found.", 
                    "Use \'BiocManager::install(\"ComplexHeatmap\") to install it."))
    
    if (!requireNamespace("grid", quietly = TRUE))
        stop(paste("Required package \'grid\' not found.", 
                    "Use \'BiocManager::install(\"grid\") to install it."))
        
    ## If dms_data argument missing
    if (missing(dms_data)) {
 
        message(paste(
            "'dms_data' not provided,",
            "using DMS data loaded with dms_substitutions()"
        ))
        dms_data <- dms_substitutions()
    } else {
        dms_data
    }
        
    ## Extract the specified assay
    assay_df <- dms_data[[assay_name]]
    
    ## Filter out multiple aa sites
    assay_df <- assay_df |>  
        filter(!grepl(":", .data$mutant))
    
    ## Stop if all rows are multiple sites
    if (nrow(assay_df) == 0){
            stop("Unable to plot DMS substitution heatmap; ", 
                "assay: '", assay_name, "' contains only ", 
                "multiple amino acid sites."
            )
    }
    
    ## Wrangle the data
    assay_df <- assay_df |>
        mutate(
        ref = str_sub(.data$mutant, 1, 1),
        pos = as.integer(
                gsub(".*?([0-9]+).*", "\\1", .data$mutant)
                ), 
        alt = str_sub(.data$mutant, -1)
        )
    
    assay_df <- assay_df |> dplyr::select("ref", "pos", "alt", "DMS_score")
    
    ## Reshape to wide format
    assay_wide <- assay_df |>
        #select(-ref) |>
        pivot_wider(names_from = .data$alt, values_from = .data$DMS_score) |>
        arrange(pos)
    
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
        select(ref, pos) |> 
        unique()
    
    ## cluster_columns with NA check
    if (sum(is.na(column_annotation)) > 0 & cluster_columns == TRUE){
            stop("Protein range includes missing values, preventing ", 
                "clustering of columns. Try setting exact_coord argument ",
                "to FALSE."
            )
    }
    
    column_annotation[is.na(column_annotation)] <- " "
    
    ## Convert to matrix
    pos <- assay_pos$pos
    alt <- colnames(assay_pos)
    alt <- alt[-c(1)]
    
    assay_pos <- assay_pos |>
        select(-c(ref))
    
    heatmap_matrix <- assay_pos |>
        select(2:length(assay_pos)) |> as.matrix()
    
    ## Set aa pos as rownames of matrix and transpose
    rownames(heatmap_matrix) <- pos
    heatmap_matrix <- t(heatmap_matrix)
    
    ## Reorder rows based on physiochemical properties
    phyiochem_order <- "DEKRHNQSTPGAVILMCFYW"
    phyiochem_order <- unlist(strsplit(phyiochem_order, split = ""))
    
    reordered_matrix <- heatmap_matrix[match(phyiochem_order, 
                                               rownames(heatmap_matrix)), ]
    
    
    ## Define a text annotation for the columns
    column_annotation <- columnAnnotation(
      text = anno_text(column_annotation$ref, 
           rot = 0, just = "right", gp = gpar(fontsize = 10))
    )

    ## Create the heatmap
    if (missing(color_scheme)) {
        color_scheme <- "default"
    }
    col_fun <- make_col_fun_dms(reordered_matrix, color_scheme)
        
    plot <- ComplexHeatmap::Heatmap(reordered_matrix,
        name = "DMS Score",
        cluster_rows = cluster_rows,
        cluster_columns = cluster_columns,
        col = col_fun,
        na_col = "grey",
        top_annotation = column_annotation,
        ...)

    return(plot)
}