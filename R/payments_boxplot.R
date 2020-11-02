#' Make a Boxplot of Payments by DRG Codes
#'
#' This function makes an interactive boxplot of payments data by DRG codes based on \code{DRG_data}.
#' @param x one of \code{"Average.Covered.Charges"}, \code{"Average.Total.Payments"} and
#' \code{"Average.Medicare.Payments"}
#' @importFrom tidyverse
#' @importFrom plotly
#' @return An interactive boxplot
#' @export
#'
#' @examples payments_boxplot("Average.Medicare.Payments")
payments_boxplot <- function(x) {
  DRG_data %>%
    select(
      DRG.Definition,
      Average.Covered.Charges,
      Average.Total.Payments,
      Average.Medicare.Payments
    ) %>%
    mutate(DRG.code = substr(DRG.Definition, 1, 3)) %>%  ##extract the code part
    rename(payment = x) -> data  ##necessary step: convert a string to variable name

  plot_ly(
    data = data,
    x =  ~ DRG.code,
    y =  ~ payment,
    type = "box",
    text =  ~ DRG.Definition,
    ##put the DRG.info in the hover instead of axis
    hoverinfo = "text"
  ) %>%
    layout(
      xaxis = list(title = "DRG.code",
                   tickangle = -70),
      yaxis = list(title = x),
      title = paste("Boxplot of", x, "by DRG.code")
    )
}
