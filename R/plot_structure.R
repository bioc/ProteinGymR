#' @rdname plot_structure
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


#' @rdname plot_structure
#'
#' @noRd
#' 
#' @importFrom dplyr filter
#'

## Extract protein from assay names
getProtIDs <- function(assay_names) {
    sapply(assay_names, function(x) {
        parts <- strsplit(x, "_", fixed = TRUE)[[1]]
        paste(parts[1:2], collapse = "_")
        })
}

#' @rdname plot_structure
#' 
#' @title Visualize DMS and Model Scores on 3D Protein Structures
#' 
#' @description `plot_structure()` plots DMS or model scores for amino acid
#'    substitutions on a 3D protein structure for a chosen assay. 
#'
#' @param assay_name `character()` a valid DMS assay name. For the full list of 
#'    available assays, run `names()` on the list object loaded with 
#'    `ProteinGymR::dms_substitutions()`. Alternatively, the name of a 
#'    user-defined DMS assay.
#'    
#' @param pdb_data `list()` object of protein structure coordinates in a
#'    Protein Data Bank format.  By default, pdb files for proteins associated
#'    with ProteinGym data can be loaded in with `ProteinGymR::pdb_files()`.
#'    Alternatively, a user-defined list of pdb data.frames with names 
#'    matching the `assay_name` param.
#'    
#' @param pdb_file `string()` input path to PBD file.
#' 
#' @param dms_data `list()` object of DMS assays loaded with 
#'   `ProteinGymR::dms_substitutions()`.
#'    Alternatively, a user-defined list of DMS assays with names corresponding
#'    to `assay_name` param.
#'
#' @param start_pos `integer()` first amino acid position to plot. If missing, 
#'    default start is the first position along the protein in the PDB file.
#'    
#' @param end_pos `integer()` last amino acid position to plot. If missing, 
#'    default end is the last position along the protein in the PDB file.
#'    
#' @param full_structure `logical()` defaults to FALSE and will only plot 
#'    protein regions where there is DMS data available in the assay. If 
#'    `start_pos` and `end_pos` coordinates are specified, plotting is 
#'    restricted to this defined region. Setting `full_structure()` to TRUE 
#'    will display full protein structure in the PBD file, and grey out regions
#'    where no DMS data is available.
#'    
#' @param aggregate_fun method for aggregating DMS scores for each residue. 
#'    For example, give [min], [max], or [var] to return the minimum, maximum, 
#'    or variance of scores for each position, respectively. `aggregate_fun` can 
#'    also take in a user-defined function with a numeric vector as input. 
#'    By default, the mean DMS score across mutations at each position is
#'    calculated.
#'
#' @details
#'
#' For `plot_structure()`, 
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
#' Each PBD table in `pdb_data()` must include the following columns:
#' 
#'
#' @return `plot_structure()` returns a [`r3dmol::r3dmol`] 
#'    object of DMS scores for each position along a protein in a chosen DMS 
#'    assay. The x-axis shows amino acid positions where a DMS mutation exist, 
#'    and the y-axis represents possible amino acid residues, ordered by default
#'    based on the physiochemical groupings. Higher and lower DMS scores 
#'    indicate a more positive or negative fitness effect after the mutation, 
#'    respectively.
#'   
#' @importFrom dplyr filter pull as_tibble rename_with mutate 
#'              arrange select
#'              
#' @importFrom ExperimentHub ExperimentHub
#' 
#' @importFrom AnnotationHub query
#'              
#' @importFrom tidyr pivot_wider
#' 
#' @importFrom bio3d read.pdb
#' 
#' @importFrom stringr str_sub
#' 
#' @importFrom r3dmol r3dmol m_zoom_to m_add_model m_remove_all_models
#'              m_style_cartoon m_set_style
#' 
#' @examples
#' 
#' # Using default dms_data
#' plot_structure(assay_name = "ACE2_HUMAN_Chan_2020")
#' 
#' # Use EVE model color scheme
#' plot_structure(assay_name = "ACE2_HUMAN_Chan_2020", 
#'     color_scheme = "EVE")
#'    
#' plot_structure(assay_name = "C6KNH7_9INFA_Lee_2018",
#'    start_pos = 20, 
#'    end_pos = 50,
#'    full_structure = FALSE,
#'    aggregate_fun = min)
#'    
#' @export 
plot_structure <- function(assay_name, 
                    pdb_file, 
                    dms_data, 
                    start_pos = NULL,
                    end_pos = NULL,
                    full_structure = FALSE,
                    aggregate_fun = mean, 
                    color_scheme) {
    
    ## Grab pdb file from ExperimentHub if not specified by user
    if (missing(pdb_file)){
        
        prot <- getProtIDs(assay_name = assay_name)
        
        eh <- ExperimentHub()
        # Grab ehid of PDB
        results <- query(eh, c("ProteinGym", prot))
        ehid <- results$ah_id
        
        # Choose one of the results (replace with an actual EH ID from the query above)
        pdb_file <- eh[[ehid]]

    } else {
        
        ## User-defined pdb_file
        pdb_file
    }

    ## Read the PDB file
    pdb <- read.pdb(pdb_file)
    
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
 
    ## Extract the DMS data for the given assay name
    df <- dms_data[[assay_name]]
    
    ## Split pos and amino acids
    df <- df |>
        mutate(
          ref = str_sub(.data$mutant, 1, 1),
          pos = as.integer(gsub(".*?([0-9]+).*", "\\1",
                                .data$mutant)),
          alt = str_sub(.data$mutant, -1)
        )
    
    ## Aggregate DMS_scores by position
    df <- df |>
        group_by(pos) |>
        summarise(
          aggregate_dms = do.call(aggregate_fun, list(DMS_score)),
          .groups = 'drop'
        )
    
    ## Select user-defined protein range
    filtered_df <- filter_by_pos(
        df = df, 
        start_pos = start_pos, 
        end_pos = end_pos
        )
    
    start_pos <- min(filtered_df$pos)
    end_pos <- max(filtered_df$pos)
    selected_residues <- list(resi = c(start_pos:end_pos))
    
    ## Normalize score between -1 and 1, centered around zero
    max_abs <- max(abs(filtered_df$aggregate_dms))
    
    filtered_df <- filtered_df |> 
        mutate(
            norm_scores = .data$aggregate_dms / max_abs
        )
    
    ## Map normalized values to a color scale
    ## Use EVE coloring
    if (missing(color_scheme)) {
        color_func <- colorRampPalette(c("red", "white", "blue"))
    } else if (color_scheme == "EVE") {
        color_func <- colorRampPalette(
            c("#000", 
            "#9440e8", 
            "#00CED1", 
            "#fde662"))
    } else {
        color_func <- colorRampPalette(c("red", "white", "blue"))
    }

    ## Generate color palette
    num_colors <- 100
    color_palette <- color_func(num_colors)
    
    ## Function to map values to color
    value_to_color <- function(values, palette) {
        scaled_index <- round((values + 1) / 2 * (length(palette) - 1) + 1)
        palette[scaled_index]
    }
  
    ## Map normalized scores to colors
    filtered_df$color <- value_to_color(filtered_df$norm_scores, color_palette)
    
    ## If full_structure missing or set to TRUE, display complete protein
    if (missing(full_structure) | full_structure == FALSE) {
         
        ## Initialize the 3D viewer, hide all but except selected regions
        viewer <- r3dmol() |>
            m_remove_all_models() |>
            m_add_model(data = pdb_file, format = "pdb") |>
            m_set_style(style = m_style_cartoon(), 
                sel = list(resi =  start_pos:end_pos)) |>
            m_zoom_to(sel = list(resi = start_pos:end_pos)) 
        
        ## Apply colors to residues with data
        for (i in 1:nrow(filtered_df)) {
        viewer <- viewer |>
            m_set_style(
                sel = list(resi = filtered_df$pos[i]),
                style = list(cartoon = list(color = filtered_df$color[i]))
              )
        }
        
        return(viewer)
    
    } else {
        
         message(paste(
            "'full_structure' is set to TRUE by default,",
            "displaying complete protein structure."
        ))
     
        ## Code to show full protein
        full_viewer <- r3dmol() |>
            m_remove_all_models() |> 
            m_add_model(data = pdb_file, format = "pdb") |> 
            m_set_style(style = m_style_cartoon())
         
        ## Get all residues in the PDB
        pdb_residues <- unique(data.frame(
             resi = pdb$atom$resno  # Residue numbers
             ))
        
        ## Identify residues without data
        residues_without_data <- setdiff(pdb_residues$resi, filtered_df$pos)
 
        ## Apply colors to residues with data
        for (i in 1:nrow(filtered_df)) {
        full_viewer <- full_viewer |>
            m_set_style(
                sel = list(resi = filtered_df$pos[i]),
                style = list(cartoon = list(color = filtered_df$color[i]))
              )
        }
        
        ## Color residues without data as yellow
        for (resi in residues_without_data) {
         full_viewer <- full_viewer |>
             m_set_style(
                 sel = list(resi = resi),
                 style = list(cartoon = list(color = "#3f3f3f"))
             )
        }
        
        return(full_viewer)
    }  
}
