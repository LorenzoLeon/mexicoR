u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Desagrupar por fila en caso de haber usado rowwise
#'
#' @param data dataframe agrupado por fila
#'
#' @return dataframe sin agrupación
#' @export
unrowwise_df <- function(data) {
  
  class(data) <- c("tbl_df", "data.frame")
  
  return(data)
  
}

#' Guardar base de datos en nuevo objeto
#'
#' @param data cualquier objeto
#' @param nombre string que se le asigna como nombre al objeto. Si no se 
#' asigna ninguno, se guardará como obj_guardado .
#'
#' @return el mismo objeto y guarda un nuevo objeto con el nombre de bd_guardada
#' @export
#'
#' @examples
guardar_obj <- function(data, nombre = "obj_guardado") {
  
  assign(nombre, 
         data,
         envir = .GlobalEnv)

  
  return(data)
  
}
  
