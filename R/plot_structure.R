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
#' @param dms_data `list()` object of DMS assays loaded with 
#'   `ProteinGymR::dms_substitutions()`.
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
#' @param aggregate_fun method for aggregating DMS scores for each residue. 
#'    For example, give min, max, or var to return the minimum, maximum, or
#'    variance of scores for each position, respectively. `aggregate_fun` can 
#'    also take in a user-defined function with a numeric vector as input. 
#'    By default, the mean score across DMS mutations at each position is
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
#' @importFrom tidyr pivot_wider
#' 
#' @importFrom bio3d read.pdb
#' 
#' @importFrom stringr str_sub
#' 
#' @importFrom r3dmol r3dmol
#' 
#' @examples
#' plot_3D(assay_name = "ACE2_HUMAN_Chan_2020", 
#'    pdb_file = "~/Desktop/R/docker-data/ProteinGym_data/ProteinGym_v1.1/ProteinGym_AF2_structures/ACE2_HUMAN_.pdb",
#'    dms_data = dms_data, 
#'    aggregate_fun = max)
#'    
#' plot_3D(assay_name = "ADRB2_HUMAN_Jones_2020", 
#'    pdb_file = "~/Desktop/R/docker-data/ProteinGym_data/ProteinGym_v1.1/ProteinGym_AF2_structures/ADRB2_HUMAN.pdb",
#'    dms_data = dms_data, 
#'    aggregate_fun = mean)
#'    
#'    
#' @export 
plot_3D <- function(assay_name, 
                    pdb_file, 
                    dms_data, 
                    aggregate_fun = mean) {
    
    # Grab pdb path
    
    # Read the PDB file
    pdb <- read.pdb(pdb_file)
    
    # Extract the DMS data for the given assay name
    df <- dms_data[[assay_name]]
    
    # Split pos and amino acids
    df <- df |>
        mutate(
          ref = str_sub(.data$mutant, 1, 1),
          pos = as.integer(gsub(".*?([0-9]+).*", "\\1",
                                .data$mutant)),
          alt = str_sub(.data$mutant, -1)
        )
    
    df <- df |>
        group_by(pos) |>
        summarise(
          aggregate_dms = do.call(aggregate_fun, list(DMS_score)),
          .groups = 'drop'
        )
    
    # Normalize score between -1 and 1, centered around zero
    max_abs <- max(abs(df$aggregate_dms))
    
    df <- df |> 
        mutate(
            norm_scores = .data$aggregate_dms / max_abs
        )
    
    # Map normalized values to a color scale
    color_func <- colorRampPalette(c("red", "white", "blue"))
    
    # Generate color palette
    num_colors <- 100
    color_palette <- color_func(num_colors)
    
    # Function to map values to color
    value_to_color <- function(values, palette) {
        scaled_index <- round((values + 1) / 2 * (length(palette) - 1) + 1)
        palette[scaled_index]
    }
  
    # Map normalized scores to colors
    df$color <- value_to_color(df$norm_scores, color_palette)

    # Initialize the 3Dmol.js viewer
    viewer <- r3dmol() |>
        m_add_model(data = pdb_file, format = "pdb") |> 
        m_zoom_to()
    
    # Get all residues in the PDB
    pdb_residues <- unique(data.frame(
        resi = pdb$atom$resno  # Residue numbers
        ))
    
    # Identify residues without data
    residues_without_data <- setdiff(pdb_residues$resi, df$pos)
    
    # Apply colors to residues with data
    for (i in 1:nrow(df)) {
    viewer <- viewer |>
        m_set_style(
            sel = list(resi = df$pos[i]),
            style = list(cartoon = list(color = df$color[i]))
          )
    }
    
    # Color residues without data as yellow
    for (resi in residues_without_data) {
    viewer <- viewer |>
        m_set_style(
            sel = list(resi = resi),
            style = list(cartoon = list(color = "#46444C"))
        )
    }
    
    # Return the viewer object
    return(viewer)
}
