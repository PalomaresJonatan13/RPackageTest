#' Boxplots
#'
#' Boxplots are plotted for the given variables in a DF
#'
#' @param df Dataframe to be used.
#' @param vars Variables used from the df to create the boxplots
#' @param ncols Number of columns in which the boxplots are going to be displayed
#' @return NULL
#' @examples
#' plot_boxplots(iris)
#' plot_boxplots(iris, "Sepal.Length")
#' plot_boxplots(iris, c("Petal.Length", "Petal.Width", "Sepal.Length"), ncols = 2)
#' @export
plot_boxplots <- function(df, vars=NULL, ncols=2) {
  if (is.null(vars)) {
    vars <- names(df)[sapply(df, is.numeric)]
  }
  if (!all(vars %in% names(df))) {
    stop("One or more specified variables are not found in the dataframe.")
  }
  numeric_vars <- vars[sapply(df[vars], is.numeric)]
  len_numeric_vars = length(numeric_vars)
  if (len_numeric_vars == 0) {
    stop("No numeric variables available for boxplot.")
  }

  ncols = min(ncols, len_numeric_vars)
  nrows <- ceiling(length(numeric_vars) / ncols)

  # Save current graphics parameters to restore later
  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  # grid layout for the plots
  par(mfrow = c(nrows, ncols), mar=c(1, 3, 1, 1), oma=c(0, 0, 0, 0))

  for (var in numeric_vars) {
    boxplot(df[[var]], main = var, ylab = var)
  }
  return(NULL)
}
