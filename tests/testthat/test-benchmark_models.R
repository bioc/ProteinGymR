test_that("check_metric_argument() works", {
    
    ## Test case when invalid metric
    expect_error(
        check_metric_argument(user_metric = "Pearson"),
        paste(
            "Invalid model\\(s\\) specified: Pearson"
        )
    )
    
    ## Test case when more than 1 metric selected
    expect_error(
        check_metric_argument(user_metric = c("AUC", "MCC")),
        paste(
            "Select only one metric for comparison"
        )
    )
})

test_that("check_model_argument() works", {
    
    ## Test case when invalid metric
    expect_error(
        check_model_argument(models = "Wrong_model"),
        paste(
            "Invalid model\\(s\\) specified: Wrong_model"
        )
    )
    
    ## Test case when more than 5 models selected
    expect_error(
        check_model_argument(
            models = c("Site_Independent", "EVmutation", "ESM_1b",
            "ProtGPT2", "Progen2_Base", "CARP_640M")
        ),
        paste(
            "Select up to 5 models for comparison"
        )
    )
})

test_that("benchmark_models() works", {
    
    ## Test case when metric not defined
    res <- evaluate_promise(benchmark_models(model = "GEMME"))
    expect_identical(
        res$messages[1],
        paste(
            "No metric specified. Using default Spearman correlation\n"
        )
    )
    
    expect_identical(
        res$result$labels$y,
        paste(
            "Spearman score"
        )
    )
    
    ## Test case when models not defined
    expect_error(
        benchmark_models(metric = "AUC"),
        paste(
            "Select at least one model from `available_models\\(\\)`"
        )
    )
    
    ## Test Spearman table is all positive values
    expect_identical(
        all(res$result$data$score >= 0),
        as.logical("TRUE")
    )
    
    ## Test MCC should be -1 to 1
    res <- evaluate_promise(benchmark_models(metric = "MCC", model = "GEMME"))
    # Range should be -0.019,  0.798
    
    object <- benchmark_models(metric = "MCC", model = "GEMME")

    
    expect_identical(
        all(res$result$data$score >= 0),
        as.logical("FALSE")
    )
    
    ## Test pivot_longer worked correctly
    expect_identical(
        tibble::is_tibble(res$result$data),
        as.logical("TRUE")
    )
    
    expect_equal(
        res$result$data |> NROW(),
        217L
    )
     
    expect_identical(
        colnames(res$result$data),
        c("model", "score", "model_mean")
    )
     
    ## Test that it created correct ggplot object
    expect_identical(
        ggplot2::ggplot_build(object)$data[[1]]$xmin |> unique(), 
        1.12
    )
    
    expect_identical(
        ggplot2::ggplot_build(object)$data[[1]]$xmax |> unique(), 
        1.72
    )
})
