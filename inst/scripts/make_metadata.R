### =========================================================================
### Make metadata
### -------------------------------------------------------------------------
###

# AlphaMissense_Supplementary
AlphaMissense_Supplementary <- data.frame(
    Title = "Cheng_ProteinGym_S8_supplementary",
    Description = paste("ProteinGym data provided by Supplementary Table S9",
                         "from Cheng et al. 2023"),
    BiocVersion = "3.20",
    Genome = "hg38",
    SourceType = "RDS",
    SourceUrl = "https://www.science.org/doi/10.1126/science.adg7492",
    SourceVersion = NA,
    Species = "Homo Sapiens",
    TaxonomyId = 9606,
    Coordinate_1_based = NA,
    DataProvider = "Cheng et al. 2023",
    Maintainer = "Tram Nguyen <Tram_Nguyen@hms.harvard.edu>",
    RDataClass = "Data.Frame",
    DispatchClass = "RDS",
    RDataPath = "ProteinGymR/Cheng_ProteinGym_S8_supplementary",
    stringsAsFactors = FALSE
)


# ProteinGym_DMS_substitutions_v1
ProteinGym_DMS_subs <- data.frame(
    Title = "ProteinGym_DMS_substitutions_v1",
    Description = paste0("ProteinGym DMS information for 216 assays",
                         "from Notin et al. 2023"),
    BiocVersion = "3.20",
    Genome = "hg38",
    SourceType = "RDS",
    SourceUrl = "https://proteingym.org/",
    SourceVersion = NA,
    Species = "Homo Sapiens",
    TaxonomyId = 9606,
    Coordinate_1_based = NA,
    DataProvider = "Marks Lab at Harvard Medical School",
    Maintainer = "Tram Nguyen <Tram_Nguyen@hms.harvard.edu>",
    RDataClass = "List",
    DispatchClass = "RDS",
    RDataPath = "ProteinGymR/ProteinGym_DMS_substitutions_v1",
    stringsAsFactors = FALSE
)

# Combined meta-data
df_all <- base::rbind(
    AlphaMissense_Supplementary, 
    ProteinGym_DMS_subs
)

# Save .csv file
write.csv(df_all, file = "inst/extdata/metadata.csv", row.names = FALSE)
