
# Loads used packages
suppressMessages({
  require(datasets)
  require(palmerpenguins, quietly = TRUE)
  require(plotly, quietly = TRUE)
  require(kableExtra, quietly = TRUE)
  require(ggforce, quietly = TRUE)
  require(olsrr)
  require(nnet)
  require(emmeans)
  require(car)
  require(lme4)
  require(tidyverse, quietly = TRUE)
})

utils::data(penguins)

# Filters away missing values
penguins <-
  penguins |>
  dplyr::filter(!is.na(sex))

modelData <-
  penguins |>
  select(!c(island, year))

simpleModel <- stats::lm(formula = bill_length_mm ~ ., data = modelData)

utils::data(anscombe)

ansLong <-
  anscombe |>
  tidyr::pivot_longer(
    cols = everything(),
    names_to = c(".value", "set"),
    names_pattern = "(.)(.)"
  )

options(knitr.kable.NA = '', knitr.table.html.attr = "quarto-disable-processing=true")

# senic <- readr::read_csv("resources/data/SENIC.csv") |>
#   dplyr::mutate(Medical_school_affiliation = ifelse(Medical_school_affiliation == 1, 1, 0) |> as.factor()) |>
#   suppressMessages()
#
# weeds <- readr::read_csv2("resources/data/weeds.csv") |>
#   rename(
#     metod = 1,
#     skörd = 2
#   ) |>
#   # Måste säkerställa att den kvalitativa variabeln är en faktor
#   mutate(
#     metod = as.factor(metod)
#   ) |>
#   suppressMessages()
#
# coffee <- readr::read_csv2("resources/data/coffee.csv") |>
#   suppressMessages()
#
# roach <- readr::read_csv2("resources/data/roaches.csv") |>
#   mutate(across(humidity:temperature, ~ as.factor(.x))) |>
#   suppressMessages()
#
# fuel <- readr::read_csv2("resources/data/fuelefficiency.csv") |>
#   mutate(across(car:driver, ~ as.factor(.x))) |>
#   suppressMessages()

#' Custom function diagnosticPlots that generate visualizations of residuals
#' @param model A fitted model object of class "lm"
#' @param alpha Defines the opacity of the points (0-1)
#' @param bins  Defines the number of bins in the histogram
#' @param scaleLocation Boolean if a scale location graph should be added

diagnosticPlots <- function(model, alpha = 1, bins = 10, scaleLocation = FALSE) {
  if (alpha < 0 | alpha > 1) {
    stop("alpha must be between 0 and 1")
  }
  if (bins <= 0) {
    stop("bins must be a positive number")
  }

  # Summarizes the residuals, observed and fitted values in a tibble
  residualData <-
    dplyr::tibble(
      residuals = residuals(model),
      # The response variable is the first column in the model's model object
      y = model$model[,1],
      yHat = fitted(model)
    )

  # Generates the histogram to assess normality
  p1 <-
    ggplot2::ggplot(residualData) +
    ggplot2::aes(x = residuals, y = after_stat(density)) +
    ggplot2::geom_histogram(bins = bins, fill = "steelblue", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Residuals", y = "Density")

  # Generates the scatter plot to assess constant variance
  p2 <-
    ggplot2::ggplot(residualData) +
    ggplot2::aes(x = yHat, y = residuals) +
    ggplot2::geom_hline(aes(yintercept = 0)) +
    ggplot2::geom_point(color = "steelblue", alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(x = "Estimated Values", y = "Residuals")

  # Generates the QQ plot to assess normality
  p3 <-
    ggplot2::ggplot(residualData) +
    # Use standardized residuals
    ggplot2::aes(sample = scale(residuals)) +
    ggplot2::geom_qq_line() +
    ggplot2::geom_qq(color = "steelblue", alpha = alpha) +
    ggplot2::theme_bw() +
    ggplot2::labs(x= "Theoretical Quantiles", y = "Observed Quantiles")

  # If scaleLocation is TRUE, add a scale location plot
  if (scaleLocation) {
    p4 <-
      ggplot2::ggplot(residualData) +
      ggplot2::aes(x = yHat, y = sqrt(abs(residuals))) +
      ggplot2::geom_point(color = "steelblue", alpha = alpha) +
      ggplot2::theme_bw() +
      ggplot2::labs(x = "Estimated Values", y = expression(sqrt("|Residuals|")))

    cowplot::plot_grid(p1, p2, p3, p4, nrow = 2)

  } else {
    cowplot::plot_grid(p1, p2, p3, nrow = 2)
  }

}
