#' Phi Correlation
#'
#' Calculates the phi correlation between dichotomous variables and displays the results
#' with a heatmap.
#'
#' @param data Dataset to which the variables belong.
#' @param asig A list that can be defined by the user if they wish to specify a value for 0 and 1.
#' @return Heatmap where the correlation between the categorical variables is shown.
#' @examples
#' data <- data.frame(
#' smoke = c("Smokes", "Smokes", "Smokes", "Does not smoke", "Smokes"),
#' diabetes = c("No", "No", "Yes", "Yes", "No"),
#' hypertension = c("Yes", "No", "Yes", "No", "Yes")
#' )
#' asig <- list(smoke = c("Does not smoke", "Smokes"), diabetes = c("No", "Yes"))
#' phi_cor(data, asig)
#' @export
phi_cor <- function(data, asig = NULL) {
  # Detectar variables dicotómicas
  bin_var <- sapply(data, function(x) {
    if (is.numeric(x) || is.factor(x) || is.character(x)) {
      length(unique(x)) == 2
    } else {
      FALSE
    }
  })

  # Filtrar por variables seleccionadas en asig (si no es NULL)
  if (!is.null(asig)) {
    bin_var <- bin_var & names(data) %in% asig
  }

  # Verificar si hay variables dicotómicas
  if (!any(bin_var)) {
    stop("Error: No dichotomous variables were found in the data set.")
  }

  # Crear un nuevo dataframe con las variables binarias convertidas a 0 y 1
  bin_data <- data.frame(lapply(names(data)[bin_var], function(var) {
    levels_order <- if (!is.null(asig) && var %in% names(asig)) asig[[var]] else sort(unique(data[[var]]))
    as.numeric(factor(data[[var]], levels = levels_order)) - 1
  }))
  colnames(bin_data) <- names(data)[bin_var]

  # Matriz Phi
  n <- sum(bin_var)
  phi_matrix <- matrix(NA, n, n)
  colnames(phi_matrix) <- rownames(phi_matrix) <- names(bin_data)

  # Cálculo de Phi
  for (i in 1:n) {
    for (j in 1:n) {
      t <- table(factor(bin_data[[i]], levels = c(0, 1)),
                 factor(bin_data[[j]], levels = c(0, 1)))

      if (all(dim(t) == c(2, 2))) {
        phi_matrix[i, j] <- phi(t)
      } else {
        phi_matrix[i, j] <- NA
      }
    }
  }

  # Graficar la matriz Phi
  corrplot(phi_matrix, method = "color", type = "full",
           tl.col = "black", tl.srt = 45, diag = TRUE,
           col = colorRampPalette(c("darkblue", "white", "darkred"))(200),
           addCoef.col = "black", number.cex = 0.8,
           cl.cex = 0.8,
           cl.ratio = 0.2,
           cl.length = 5)

  return(phi_matrix)
}
