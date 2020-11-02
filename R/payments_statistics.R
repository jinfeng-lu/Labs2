#' Calculates Statistics over DRG codes for Payments
#'
#' This function calculates several statistics for payments data by DRG codes based on \code{DRG_data}.
#' @param x one of \code{"Average.Covered.Charges"}, \code{"Average.Total.Payments"} and
#' \code{"Average.Medicare.Payments"} (default value)
#' @param op one of statistics \code{mean}, \code{median}, and \code{sd}
#'
#' @return A descriptive string about the statistics
#' @export
#'
#' @examples payments_statistics(op="mean")
payments_statistics <- function(x = "Average.Medicare.Payments", op) {
  DRG_data %>%
    select(
      DRG.Definition,
      Average.Covered.Charges,
      Average.Total.Payments,
      Average.Medicare.Payments
    ) %>%
    mutate(DRG.code = substr(DRG.Definition, 1, 3)) %>%  ##extract the code part
    rename(payment = x) -> data    ##necessary step: convert a string to variable name

  result <- switch(
    op,
    mean = mean(data$payment),    ##switch() takes name as input
    median = median(data$payment),
    sd = sd(data$payment)
  ) %>% round(2)
  paste0(
    "The ",
    ifelse(op == "sd", "standard deviation", op),  ##make sd more friendly for users
    " of ",
    str_replace_all(x, "\\.", " "),
    " is ",
    result,
    "$."
  )
}
