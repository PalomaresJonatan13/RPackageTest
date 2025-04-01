#' Frequency and Contingency Tables for a Variable
#'
#' Creates frequency tables for one variable or others chosen by the user, if desired.
#'
#' @param data Dataset to which the variables belong.
#' @param var Variable 1 for which the frequency table is created.
#' @param vec Vector of variables with which contingency tables are created.
#' @return Frequency table for the given variable and contingency tables for other variables.
#' @examples
#' tfreq(iris, "Species")$freq_table;
#' @export
tfreq <- function(data, var, vec = NULL) {
  # Frequency table for the main variable
  freq_table <- data %>%
    count(.data[[var]]) %>%
    mutate(RelativeFrequency = n / sum(n)) %>%
    mutate(PercentageFrequency = RelativeFrequency * 100)

  tables <- list()

  if (!is.null(vec)) {
    N <- length(vec)
    for (i in 1:N) {
      if (vec[i] %in% colnames(data)) {
        # Frequency tables for selected variables
        abs_freq <- data %>%
          count(.data[[var]], .data[[vec[i]]]) %>%
          pivot_wider(names_from = .data[[vec[i]]], values_from = n, values_fill = list(n = 0))

        pct_freq <- data %>%
          count(.data[[var]], .data[[vec[i]]]) %>%
          group_by(.data[[var]]) %>%
          mutate(perc = (n / sum(n)) * 100) %>%
          ungroup() %>%
          select(-n) %>%
          pivot_wider(names_from = .data[[vec[i]]], values_from = perc, values_fill = list(perc = 0))

        tables[[vec[i]]] <- list(abs_freq = abs_freq, pct_freq = pct_freq)
      } else {
        print(paste("Variable", vec[i], "not found in the dataset."))
      }
    }
  }
  return(list(freq_table = freq_table, frequency_tables = tables))
}

