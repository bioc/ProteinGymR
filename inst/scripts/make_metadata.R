### =========================================================================
### Make metadata
### -------------------------------------------------------------------------
###

library(tools)

# AlphaMissense_Supplementary
AlphaMissense_Supplementary <- data.frame(
    Title = "AlphaMissense pathogenicity scores for variants in ProteinGym",
    Description = paste("Supplementary table from Cheng et al. 2023 containing",
                        "AlphaMissense pathogenicity scores for mutations",
                        "found in ProteinGym DMS substitution data"),
    BiocVersion = "3.20",
    Genome = NA,
    SourceType = "RDS",
    SourceUrl = "https://www.science.org/doi/10.1126/science.adg7492",
    SourceVersion = NA,
    Species = NA,
    TaxonomyId = NA,
    Coordinate_1_based = TRUE,
    DataProvider = "Cheng et al. 2023",
    Maintainer = "Tram Nguyen <Tram_Nguyen@hms.harvard.edu>",
    RDataClass = "Data.Frame",
    DispatchClass = "RDS",
    RDataPath = "ProteinGymR/Cheng_ProteinGym_variants.rds",
    stringsAsFactors = FALSE
)

# DMS_substitutions_scores
DMS_substitutions_scores <- data.frame(
    Title = "ProteinGym deep mutational scanning (DMS) assays for substitutions",
    Description = paste("ProteinGym DMS information for 217 assays",
                         "from Notin et al. 2023"),
    BiocVersion = "3.20",
    Genome = NA,
    SourceType = "RDS",
    SourceUrl = "https://proteingym.org/",
    SourceVersion = NA,
    Species = NA,
    TaxonomyId = NA,
    Coordinate_1_based = TRUE,
    DataProvider = "Marks Lab at Harvard Medical School",
    Maintainer = "Tram Nguyen <Tram_Nguyen@hms.harvard.edu>",
    RDataClass = "List",
    DispatchClass = "RDS",
    RDataPath = "ProteinGymR/ProGym217_DMS_subs_v1.1.rds",
    stringsAsFactors = FALSE
)

# DMS_sub_reference
DMS_sub_reference <- data.frame(
    Title = "ProteinGym metadata for 217 DMS substitution assays",
    Description = paste("Reference file for ProteinGym v1.1 217 DMS assays",
                         "from Notin et al. 2023"),
    BiocVersion = "3.20",
    Genome = NA,
    SourceType = "RDS",
    SourceUrl = "https://proteingym.org/",
    SourceVersion = NA,
    Species = NA,
    TaxonomyId = NA,
    Coordinate_1_based = TRUE,
    DataProvider = "Marks Lab at Harvard Medical School",
    Maintainer = "Tram Nguyen <Tram_Nguyen@hms.harvard.edu>",
    RDataClass = "Data.Frame",
    DispatchClass = "RDS",
    RDataPath = "ProteinGymR/ref_file_217_DMS_subs_v1.1.rds",
    stringsAsFactors = FALSE
)

# Zeroshot_scores_DMS_subs
zeroshot_scores_DMS_subs <- data.frame(
    Title = "ProteinGym zero-shot DMS substitution benchmarks",
    Description = paste("Zero-shot DMS substitution benchmarks from Notin et",  
                        "al. 2023 using Spearman, NDCG, AUC, MCC, and Top-K",
                        "recall metrics"),
    BiocVersion = "3.20",
    Genome = NA,
    SourceType = "RDS",
    SourceUrl = "https://proteingym.org/",
    SourceVersion = NA,
    Species = NA,
    TaxonomyId = NA,
    Coordinate_1_based = TRUE,
    DataProvider = "Marks Lab at Harvard Medical School",
    Maintainer = "Tram Nguyen <Tram_Nguyen@hms.harvard.edu>",
    RDataClass = "List",
    DispatchClass = "RDS",
    RDataPath = "ProteinGymR/zeroshot_DMS_subs_v1.1.rds",
    stringsAsFactors = FALSE
)

# Function to generate metadata for each PDB files 
generate_pdb_metadata <- function(directory) {
    # Get list of .pdb files in the directory
    pdb_files <- list.files(directory, pattern = "\\.pdb$", full.names = FALSE)
      
    # Initialize an empty list to store metadata entries
    metadata_list <- list()
      
    # Iterate through each .pdb file
    for (file in pdb_files) {
        # Remove .pdb extension for RDataPath
        rdata_path <- paste0("ProteinGymR/", file_path_sans_ext(file), ".pdb")

        # Create metadata entry
        metadata_entry <- data.frame(
            Title = paste("Protein structure for", file_path_sans_ext(file)),
            Description = paste("AlphaFold2 predicted protein structure for", 
                file_path_sans_ext(file), 
                "from ProteinGym v1.2 curated by Notin et al. 2023"),
            BiocVersion = "3.21",
            Genome = NA,
            SourceType = "PDB",
            SourceUrl = "https://zenodo.org/records/14997691",
            SourceVersion = NA,
            Species = NA,
            TaxonomyId = NA,
            Coordinate_1_based = TRUE,
            DataProvider = "Marks Lab at Harvard Medical School",
            Maintainer = "Tram Nguyen <Tram_Nguyen@hms.harvard.edu>",
            RDataClass = "Character",
            DispatchClass = "FilePath",
            RDataPath = rdata_path,
            stringsAsFactors = FALSE
        )
        
        # Add to list
        metadata_list[[file]] <- metadata_entry
    }
    
    # Combine all metadata entries into a single data frame
    metadata_df <- do.call(rbind, metadata_list)
    rownames(metadata_df) <- NULL
      
    return(metadata_df)
}


pdb_files <- generate_pdb_metadata("~/ProteinGym_data/ProteinGym_v1.2/ProteinGym_AF2_structures/")


# Combined meta-data
df_all <- base::rbind(
    AlphaMissense_Supplementary, 
    DMS_substitutions_scores,
    zeroshot_scores_DMS_subs,
    DMS_sub_reference,
    pdb_files
)

# Save .csv file
write.csv(df_all, file = "inst/extdata/metadata.csv", row.names = FALSE)
