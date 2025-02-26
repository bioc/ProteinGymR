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
#' @importFrom ComplexHeatmap Heatmap columnAnnotation anno_text 
#' 
#' @importFrom grid gpar 
#' 
#' @importFrom circlize colorRamp2
#' 
#' @importFrom stringr str_sub
#' 
#' @importFrom r3dmol r3dmol
#'
#' @export 
plot_3D <- function(assay_name, pdb_data, dms_data) {
  library(bio3d)
  library(dplyr)
  library(r3dmol)
  
  # Read the PDB file
  pdb <- read.pdb(pdb_file)
  
  # Extract the DMS data for the given assay name
  df <- dms_data[[assay_name]]
  
  # Process the data
  df <- df |>
    mutate(
      ref = str_sub(.data$mutant, 1, 1),
      pos = as.integer(gsub(".*?([0-9]+).*", "\\1", .data$mutant)),
      alt = str_sub(.data$mutant, -1)
    ) |>
    group_by(pos) |>
    summarise(
      avg_dms = mean(DMS_score),
      .groups = 'drop'
    )
  
  # Define a color gradient function for diverging scores
  score_to_color <- colorRampPalette(c("blue", "white", "red"))
  
  # Normalize scores symmetrically around 0
  max_abs_score <- max(abs(df$avg_dms))
  df$normalized_score <- (df$avg_dms + max_abs_score) / (2 * max_abs_score)
  
  # Map normalized scores to colors
  df$color <- score_to_color(100)[as.numeric(cut(df$normalized_score, breaks = 100))]
  
  # Initialize the 3Dmol.js viewer
  viewer <- r3dmol() %>%
    m_add_model(data = pdf_file, format = "pdb") %>%
    m_zoom_to()
  
  # Get all residues in the PDB
  pdb_residues <- unique(data.frame(
    resi = pdb$atom$resno  # Residue numbers
  ))
  
  # Identify residues without data
  residues_without_data <- setdiff(pdb_residues$resi, df$pos)
  
  # Apply colors to residues with data
  for (i in 1:nrow(df)) {
    viewer <- viewer %>%
      m_set_style(
        sel = list(resi = df$pos[i]),
        style = list(cartoon = list(color = df$color[i]))
      )
  }
  
  # Color residues without data as yellow
  for (resi in residues_without_data) {
    viewer <- viewer %>%
      m_set_style(
        sel = list(resi = resi),
        style = list(cartoon = list(color = "yellow"))
      )
  }
  
  # Return the viewer object
  return(viewer)
}