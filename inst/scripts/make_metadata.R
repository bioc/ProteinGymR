### =========================================================================
### Make metadata
### -------------------------------------------------------------------------
###

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
    RDataPath = "ProteinGymR/ProGym217_DMS_subs_v1.rds",
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
    RDataPath = "ProteinGymR/ref_file_217_DMS_subs_v1.rds",
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
    RDataPath = "ProteinGymR/zeroshot_DMS_subs_v1.rds",
    stringsAsFactors = FALSE
)

# Combined meta-data
df_all <- base::rbind(
    AlphaMissense_Supplementary, 
    DMS_substitutions_scores,
    zeroshot_scores_DMS_subs,
    DMS_sub_reference
)

# Save .csv file
write.csv(df_all, file = "inst/extdata/metadata.csv", row.names = FALSE)
