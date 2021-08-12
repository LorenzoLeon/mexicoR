u <- Sys.setlocale("LC_ALL", "es_ES.UTF-8")

#' Deflactar montos 
#' 
#' Esa función deflacta montos descargando la última versión de deflactores publicada por Transparencia presupuestaria.
#'
#' @param monto monto que se quiere deflactar
#' @param year_monto año de origen del monto (para que funciones tiene que ser
#' entre 1994 y 2030)
#' @param year_out año del precio al que se quiere deflactar (para que funciones tiene que ser
#' entre 1994 y 2030)
#'
#' @importFrom magrittr %>%
#' @return regresa un vector de los montos deflactados
#' @export
deflactar_tp <- function(monto, year_monto, year_out) {
  
  if (year_monto > 2030 |
      year_monto < 1994) {
    
    warning("year_monto supera el rango de años disponible. La cifra no fue deflactada.")
    
    return(monto)
    
    break
    
    }
  
  if (year_out > 2030 |
      year_out < 1994) {
    warning("year_out supera el rango de años disponible. La cifra no fue deflactada.")
    
    return(monto)
    
    break
    
  }
  
  if (year_out == year_monto) {
    warning("year_out y year_monto son iguales.")
    
    return(monto)
    
    break
    
  }
  
  if (!exists("deflactor") || 
      !(c("year") %in% colnames(deflactor)) ||
      !(c("deflactor_2013") %in% colnames(deflactor))) {
    
    temp = tempfile(fileext = ".xlsx")
    
    dataURL <- "https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/Presupuesto/Programacion/Deflactores/Deflactores_PIB.xlsx"
    
    download.file(dataURL, destfile = temp, mode = 'wb')
    
    deflactor <<- readxl::read_excel(temp,
                             range = "B4:O38") %>%
      janitor::clean_names() %>% 
      dplyr::rename(deflactor_year = starts_with("deflactor_del_pib")) %>% 
      dplyr::transmute(year = as.numeric(periodo),
                       deflactor_year) %>% 
      dplyr::filter(!is.na(year)) 
    
  }
  
  deflactor_out <- try(purrr::map_dbl(year_out,
                              function(x)deflactor[deflactor$year %in% as.numeric(x), ]$deflactor_year),
                       silent = T)
  
  if(class(deflactor_out) == "try-error"){deflactor_out <- 1}
  
  deflactor_monto <- try(purrr::map_dbl(year_monto,
                                function(x)deflactor[deflactor$year %in% as.numeric(x), ]$deflactor_year),
                         silent = T)
  
  if(class(deflactor_monto) == "try-error"){deflactor_monto <- 1}
  
  monto_deflactado <- (monto*deflactor_out)/deflactor_monto
  
  return(monto_deflactado)

}

