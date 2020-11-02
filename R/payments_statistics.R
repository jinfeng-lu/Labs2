#' Calculates Statistics over DRG codes for Payments
#'
#' This function calculates several statistics for payments data by DRG codes based on \code{DRG_data}.
#' @param x one of \code{"Average.Covered.Charges"}, \code{"Average.Total.Payments"} and
#' \code{"Average.Medicare.Payments"} (default value)
#' @param op one of statistics \code{mean}, \code{median}, and \code{sd}
#' @importFrom tidyverse
#' @return A tibble
#' @export
#'
#' @examples payments_statistics(op="mean")
payments_statistics <-
  function(x = "Average.Medicare.Payments", op) {
    DRG_data %>%
      select(
        DRG.Definition,
        Average.Covered.Charges,
        Average.Total.Payments,
        Average.Medicare.Payments
      ) %>%
      rename(payment = x) -> data    ##necessary step: convert a string to variable name

    data %>%
      group_by(DRG.Definition) %>%
      summarise(op = switch(         ##switch() takes name as input
        op,
        mean = round(mean(payment), 2),
        median = median(payment),
        sd = round(sd(payment), 2)
      )) -> result

    names(result)[2] <-
      ifelse(op == "sd",
             paste(x,"standard deviation",sep = "_"),
             paste(x,op,sep = "_")) ##rename to corresponding stat
    return(result)
  }
