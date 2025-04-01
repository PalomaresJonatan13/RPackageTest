#' Pairplot
#'
#' A pairplot is displayed for with the attributes defined by the user.
#'
#' @param df Dataframe to be used.
#' @param vars Variables used from the df to create the pairplot
#' @param method Type of correlation: Pearson, Spearman, Kendall, Bicor
#' @param diag_fun Type of plot used for the diagonal of the paiplot
#' @param lower_fun Type of plot used for the lower triangular part of the pairplot.
#' @return A list with the plot (and its information) and the correlation DF.
#' @examples
#' result <- pairplot(iris, method = "kendall", diag_fun = "densityDiag", lower_fun = "points")
#' head(result$correlation_df)
#' result$plot$data
#' @export
pairplot <- function(df, vars=NULL,
                     method="pearson",
                     diag_fun="densityDiag",
                     lower_fun="points",
                     ...) {
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  if (!all(vars %in% names(df))) {
    stop("Some of the specified variables are not present in the dataframe.")
  }
  numeric_vars <- vars[sapply(df[vars], is.numeric)]
  if (length(numeric_vars) == 0) {
    stop("No numeric variables found among the specified variables.")
  }

  method_lower <- tolower(method)
  df_subset <- df[, numeric_vars, drop = FALSE]
  if (method_lower %in% c("bicor", "biweight", "biweight midcorrelation")) {
    cor_mat <- WGCNA::bicor(as.matrix(df_subset), use = "pairwise.complete.obs")
  } else {
    cor_mat <- cor(df_subset, method = method_lower, use = "pairwise.complete.obs")
  }
  cor_df <- as.data.frame(cor_mat)

  upper_corr_heatmap <- function(data, mapping, ...) {
    var_x <- rlang::as_label(mapping$x)
    var_y <- rlang::as_label(mapping$y)

    corr_value <- cor_mat[var_x, var_y]
    df_tile <- data.frame(x = 1, y = 1, corr = corr_value)

    p <- ggplot(df_tile, aes(x = x, y = y, fill = corr)) +
      geom_tile() +
      geom_text(aes(label = sprintf("%.3f", corr)), color = "black", size = 4) +
      scale_fill_gradient2(low = "#000a", mid = "white", high = "#000a",
                           midpoint = 0, limits = c(-1, 1)) +
      theme_void() +
      theme(legend.position = "none")
    return(p)
  }

  p <- GGally::ggpairs(
    data = df,
    columns = numeric_vars,
    diag = list(continuous = diag_fun),
    lower = list(continuous = lower_fun),
    upper = list(continuous = upper_corr_heatmap),
    ...
  )
  print(p)

  return(list(plot = p, correlation_df = cor_df))
}
