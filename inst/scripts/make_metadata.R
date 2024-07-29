### =========================================================================
### Make metadata
### -------------------------------------------------------------------------
###

# AlphaMissense_Supplementary
AlphaMissense_Supplementary <- data.frame(
    Title = "AlphaMissense pathogenicity scores for variants in ProteinGym",
    Description = paste("Supplementary table from Cheng et al. 2023 containing",
                        "AlphaMissense pathogenicity scores for variants",
                        "found in ProteinGym"),
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

# ProteinGym_DMS_substitutions
ProteinGym_DMS_subs <- data.frame(
    Title = "ProteinGym deep mutational scanning (DMS) assays for substitutions",
    Description = paste("ProteinGym DMS information for 216 assays",
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
    RDataPath = "ProteinGymR/ProGym216_DMS_subs_v1.rds",
    stringsAsFactors = FALSE
)

# Combined meta-data
df_all <- base::rbind(
    AlphaMissense_Supplementary, 
    ProteinGym_DMS_subs
)

# Save .csv file
write.csv(df_all, file = "inst/extdata/metadata.csv", row.names = FALSE)
