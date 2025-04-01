#' Descriptive statistics
#'
#' Calculates the following descriptive statistics to return them in a DF: mean,
#' standard deviation, variance, minimum, quartile 1, quartile 2, quartile 3,
#' maximum, kurtosis, skewness, count of missing data, IQR and count
#'
#' @param df Dataframe for which the description is applied.
#' @return Dataframe with the descriptive statistics.
#' @examples
#' describe_df(iris)
#' @export
describe_df <- function(df) {
  numeric_cols <- sapply(df, is.numeric)
  df_numeric <- df[, numeric_cols, drop = FALSE]

  summary_table <- data.frame(
    Variable = character(),
    Mean = numeric(),
    SD = numeric(),
    Variance = numeric(),
    Min = numeric(),
    Q1 = numeric(),
    Q2 = numeric(),
    Q3 = numeric(),
    Max = numeric(),
    Kurtosis = numeric(),
    Skewness = numeric(),
    Missing = integer(),
    IQR = numeric(),
    Count = integer()
  )

  for (var in names(df_numeric)) {
    x <- df_numeric[[var]]
    summary_table <- rbind(summary_table, data.frame(
      Variable = var,
      Mean = mean(x, na.rm = TRUE),
      SD = sd(x, na.rm = TRUE),
      Variance = var(x, na.rm = TRUE),
      Min = min(x, na.rm = TRUE),
      Q1 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
      Q2 = median(x, na.rm = TRUE),
      Q3 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
      Max = max(x, na.rm = TRUE),
      Kurtosis = kurtosis(x, na.rm = TRUE),
      Skewness = skewness(x, na.rm = TRUE),
      Missing = sum(is.na(x)),
      IQR = IQR(x, na.rm = TRUE),
      Count = sum(!is.na(x))
    ))
  }

  return(summary_table)
}
