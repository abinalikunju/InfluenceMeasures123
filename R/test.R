library()
test_that("calculate_and_plot_influence works correctly", {
  data(mtcars)
  model <- lm(mpg ~ wt, data = mtcars)

  result <- calculate_and_plot_influence(mtcars, model)
  expect_type(result, "list")
  expect_length(result$influence_values, nrow(mtcars))
  expect_s3_class(result$plot, "recordedplot")

  expect_error(calculate_and_plot_influence(mtcars, "not a model"))
  expect_error(calculate_and_plot_influence(mtcars, model, measure = "invalid"))
  expect_error(calculate_and_plot_influence(mtcars, model, plot_type = "invalid"))
})
