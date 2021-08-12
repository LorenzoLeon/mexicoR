u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Desagrupar por fila en caso de haber usado rowwise
#'
#' @param x dataframe agrupado por fila
#'
#' @return dataframe sin agrupación
#' @export
ungroup.rowwise_df <- function(x) {
  class(x) <- c( "tbl_df", "data.frame")
  x
}