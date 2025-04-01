#' Charts for categorical variables
#'
#'
#' Creates a bar chart and a pie chart for a categorical variable, if the user wishes
#' it also creates a stacked bar chart between the given variables.
#' @param data Dataset to which the variables belong.
#' @param var1 Variable 1 for which the bar and pie charts are created.
#' @param var2 Variable 2 for which the stacked bar chart is created with variable 1.
#' @return Bar chart and pie chart, as applicable.
#' @examples
#' diagrams_edacat <- graph_cat(iris, "Species");
#' diagram_ab <- graph_cat(farm, "animals", "food");
#' @export
graph_cat <- function(data, var1, var2 = NULL) {
  if (!is.data.frame(data)) {
    stop("Error: The input data is not a dataframe.")
  }

  # Bar chart
  bar_chart <- ggplot(data, aes(x = .data[[var1]], fill = .data[[var1]])) +
    geom_bar() +
    scale_fill_brewer(palette = "Reds") +
    theme_minimal()
  print(bar_chart)

  # Pie chart
  pie_data <- data %>%
    group_by(.data[[var1]]) %>%
    count() %>%
    ungroup() %>%
    mutate(pcnt = `n` / sum(`n`)) %>%
    arrange(pcnt) %>%
    mutate(labels = scales::percent(pcnt))

  pie_chart <- ggplot(pie_data, aes(x = "", y = pcnt, fill = .data[[var1]])) +
    geom_col() +
    scale_fill_brewer(palette = "Greens") +
    geom_label(aes(label = labels),
               position = position_stack(vjust = 0.5),
               show.legend = FALSE) +
    coord_polar(theta = "y")
  print(pie_chart)

  # Stacked bar chart if variable2 is provided
  if (!is.null(var2)) {
    stacked_bar_chart <- ggplot(data, aes(x = .data[[var1]], fill = .data[[var2]])) +
      geom_bar() +
      scale_fill_brewer(palette = "Blues") +
      theme_minimal()
    print(stacked_bar_chart)
    return(list(bar_chart, pie_chart, stacked_bar_chart))
  }

  return(list(bar_chart, pie_chart))
}


