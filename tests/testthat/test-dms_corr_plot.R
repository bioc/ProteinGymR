library(testthat)
library(ProteinGymR)

# Invalid uniprot inputs
test_that("dms_corr_plot() validates inputs correctly", {
    expect_error(
        dms_corr_plot(uniprotId = NULL),
        regexp = "is.character\\(uniprotId\\) is not TRUE"
    )
    
    expect_error(
        dms_corr_plot(uniprotId = 12345),  
        regexp = "is.character\\(uniprotId\\) is not TRUE"
    )
})

test_that("dms_corr_plot() triggers error for invalid uniprot", {
    expect_error(
        dms_corr_plot(uniprotId = "fake_id"),
        regexp = "UniProt: 'fake_id' is not valid; check that the UniProt ID is correct"
    )
})

# Invalid model specified
test_that("dms_corr_plot() triggers error with invalid model", {
    expect_error(
        dms_corr_plot(uniprotId = "P04637", 
            model = "fake_model"), 
        regexp = "Invalid model\\(s\\) specified: fake_model"
    )
})

# No common variants to merge table
test_that("pg_match_id throws error when UniProt_id values do not match", {
    model_table <- data.frame(UniProt_id = c("P12345", "Q67890"))
    pg_table <- data.frame(UniProt_id = c("P12345", "A11111"))

    expect_error(
        pg_match_id(model_table = model_table, pg_table = pg_table),
        regexp = "unique\\(pg_table\\$UniProt_id\\) are not all TRUE"
    )
})

# Returns a ggplot object
test_that("dms_corr_plot() returns a ggplot object", {
    plot <- dms_corr_plot(uniprotId = "P04637", model = "Kermut")
    expect_s3_class(plot, "ggExtraPlot")
})