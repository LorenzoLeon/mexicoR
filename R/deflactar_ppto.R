u <- Sys.setlocale("LC_TIME", "Spanish")

#' Deflactar montos con deflactor de Transparencia Presupuestaria
#'
#' @param monto monto que se quiere deflactar
#' @param year_monto año de origen del monto
#' @param year_out año del precio al que se quiere deflactar
#'
#' @importFrom magrittr %>%
#' @return regresa un vector de los montos deflactados
#' @export
deflactar_tp <- function(monto, year_monto, year_out) {
  
  if (!exists("deflactor") || 
      !(c("year") %in% colnames(deflactor)) ||
      !(c("deflactor_2013") %in% colnames(deflactor))) {
    
    temp = tempfile(fileext = ".xlsx")
    dataURL <- "https://www.transparenciapresupuestaria.gob.mx/work/models/PTP/Presupuesto/Programacion/Deflactores/Deflactores_PIB.xlsx"
    download.file(dataURL, destfile = temp, mode = 'wb')
    
    deflactor <<- readxl::read_excel(temp,
                             skip = 3) %>% 
      janitor::clean_names() %>% 
      dplyr::transmute(year = as.numeric(periodo), 
                # cres_deflactror = crecimiento_anual_deflactor_del_pib,
                deflactor_2013 = deflactor_del_pib_base_2013) %>% 
      dplyr::filter(!is.na(year)) 
    
  }
  
  deflactor_out <- deflactor[deflactor$year == as.numeric(year_out), ]$deflactor_2013
  deflactor_monto <- deflactor[deflactor$year == as.numeric(year_monto),]$deflactor_2013
  
  monto_deflactado <- (monto*deflactor_out)/deflactor_monto
  
  return(monto_deflactado)

}

deflactar_tp <- Vectorize(deflactar_tp)

