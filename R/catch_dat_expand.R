#' Title catch_dat_expand
#'
#' Expand data.frame by replicating rows according to `CANoAtLngt`
#' @param x data.frame containing survay data
#' @param quarter column name containing the quarter of the survey as string
#' @param age column name containing fish age class as string
#' @param length_class column name containing fish length class as string
#' @param CANoAtLngt
#'
#' @return expanded data.frame with rows replicated according to the value `CANoAtLngt`
#' @export
#'
#' @examples
catch_dat_expand <- function(x, quarter, age, length_class, CANoAtLngt) {
    x <- as.data.frame(x)
    x[rep(seq(nrow(x)), x[,CANoAtLngt]), c(quarter, age, length_class)]
}
