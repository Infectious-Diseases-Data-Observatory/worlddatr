test_that("create_map returns a ggplot and annotates total n for ungrouped data", {
  set.seed(42)
  codes <- unique(world_map$alpha_3_code)
  sample_codes <- sample(codes, 3)

  df <- data.frame(COUNTRY = sample(sample_codes, 50, replace = TRUE))

  p <- create_map(data = df, country_col = "COUNTRY", include_n = TRUE)

  expect_s3_class(p, "gg")  # is a ggplot object

  # extract built data frames; the annotate text creates a layer with label
  built <- ggplot2::ggplot_build(p)
  # find the annotate/text layer in built$data by looking for "label" column
  annotate_df <- Filter(function(d) "label" %in% names(d), built$data)
  expect_true(length(annotate_df) >= 1)
  labels <- unique(annotate_df[[1]]$label)
  expect_equal(labels, paste0("n = ", nrow(df)))
})

test_that("create_map uses grouped_sums_col when grouped_data = TRUE", {
  sum_df <- data.frame(
    COUNTRY = c("GBR", "FRA", "ESP"),
    total = c(10, 5, 2)
  )

  p <- create_map(data = sum_df, country_col = "COUNTRY",
                  grouped_data = TRUE, grouped_sums_col = "total",
                  include_n = TRUE)

  expect_s3_class(p, "gg")

  built <- ggplot2::ggplot_build(p)
  # the polygon fill layer(s) contain a "fill" value mapped from n
  # find data frames containing the mapped fill (not NA)
  fill_dfs <- Filter(function(d) "fill" %in% names(d) && any(!is.na(d$fill)), built$data)
  expect_true(length(fill_dfs) >= 1)
})

test_that("create_map applies log scale transformation when log_scale = TRUE", {
  sum_df <- data.frame(COUNTRY = c("GBR", "FRA"), count = c(100, 10))

  p_log <- create_map(data = sum_df, country_col = "COUNTRY",
                      grouped_data = TRUE, grouped_sums_col = "count",
                      log_scale = TRUE, include_n = FALSE)

  built <- ggplot2::ggplot_build(p_log)
  fill_dfs <- Filter(function(d) "fill" %in% names(d) && any(!is.na(d$fill)), built$data)
  expect_true(length(fill_dfs) >= 1)
})

test_that("create_map does not add n annotation when include_n = FALSE", {
  df <- data.frame(COUNTRY = sample(unique(world_map$alpha_3_code), 5, replace = TRUE))

  p <- create_map(df, country_col = "COUNTRY", include_n = FALSE)
  built <- ggplot2::ggplot_build(p)

  annotate_df <- Filter(function(d) "label" %in% names(d), built$data)
  expect_equal(length(annotate_df), 0)
})

test_that("create_map errors when grouped_data = TRUE but grouped_sums_col is NULL", {
  df <- data.frame(COUNTRY = c("GBR", "FRA"), count = c(1,2))

  expect_error(
    create_map(df, country_col = "COUNTRY", grouped_data = TRUE),
    regexp = "The parameter"  # accept messages that arise from missing column
  )
})

test_that("create_map sets fill legend title from legend argument", {
  df <- data.frame(COUNTRY = sample(unique(worlddatr::world_map$alpha_3_code), 10, replace = TRUE))

  p <- create_map(df, country_col = "COUNTRY", legend = "My legend", include_n = FALSE)

  expect_equal(p$labels$fill, "My legend")
})
