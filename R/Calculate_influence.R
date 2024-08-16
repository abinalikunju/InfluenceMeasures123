calculate_and_plot_influence <- function(data, model, measure = "cooks", plot_type = "base") {
  if (!inherits(model, "lm")) {
    stop("Model must be an object of class 'lm'.")
  }
  if (!is.data.frame(data)) {
    stop("Data must be a data frame.")
  }
  if (any(is.na(data))) {
    stop("Data contains NA values.")
  }
  if (any(is.infinite(unlist(data)))) {
    stop("Data contains infinite values.")
  }
  if (!measure %in% c("cooks", "dffits", "hadi")) {
    stop("Invalid measure specified. Choose 'cooks', 'dffits', or 'hadi'.")
  }
  if (!plot_type %in% c("base", "ggplot")) {
    stop("Invalid plot type. Choose 'base' or 'ggplot'.")
  }
  if (nrow(data) != nrow(model.frame(model))) {
    stop("Data dimensions do not match the model.")
  }

  # Calculating influence measures
  influence_values <- switch(measure,
                             cooks = cooks.distance(model),
                             dffits = dffits(model),
                             hadi = hadi_measure(model))

  # Creating plot
  if (plot_type == "base") {
    plot_obj <- create_base_plot(influence_values, measure)
  } else {
    plot_obj <- create_ggplot(influence_values, measure)
  }

  # Returning results
  list(influence_values = influence_values, plot = plot_obj)
}
