## Test that alphamissense_table deprecated warning triggers

test_that("model_table is set to 'AlphaMissense' when alphamissense_table is used", {
  expect_warning(
    model_table_result <- dms_corr_plot_devel(
      alphamissense_table = "dummy_table",
      dms_table = "some_table",
      mode = "by_protein",
      uniprotId = "P12345"
    ),
    regexp = "Please use `model_table` = 'AlphaMissense' instead."
  )

  expect_equal(model_table_result, "AlphaMissense")
})

## Test that assay_name and uniprotId are not both accepted simultaneously
test_that("assay_name and uniprotId are not both accepted.", {
    expect_error(
        dms_corr_plot_devel(
            model_table = "GEMME",
            assay_name = "name1",
            uniprotId = "P12345",
            dms_table = "some_table",
            mode = "by_protein"
            ),
        "Please provide only one of 'uniprotId' or 'assay_name', not both.")
})


## Test when `mode` is missing
test_that("error triggers when `mode` is missing", {
    expect_error(
        dms_corr_plot_devel(
            model_table = "GEMME",
            dms_table = "some_table"
        ), 
        paste(
        "You must explicitly specify the 'mode' argument",
        "as either 'by_protein' or 'by_assay'."
        )
    )
})


## Test when `mode = "by_protein"`
test_that("that uniprotId is required when `mode = 'by_protein'`", {
    expect_error(
        dms_corr_plot_devel(
            model_table = "GEMME",
            dms_table = "some_table",
            mode = "by_protein"
        ), 
        paste(
        "When mode is 'by_protein', you must supply a 'uniprotId'."
        )
    )
})


## Test when `mode = "by_assay"`
test_that("that assay_name is required when `mode = 'by_assay'`", {
    expect_error(
        dms_corr_plot_devel(
            model_table = "GEMME",
            dms_table = "some_table",
            mode = "by_assay"
        ), 
        paste(
        "When mode is 'by_assay', you must supply an 'assay_name'."
        )
    )
})

