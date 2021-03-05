u <- Sys.setlocale("LC_TIME", "Spanish")

#' Nombre Comun completo entidad
#'
#' Transforma las abreviaciones más comunes de entidades de México en nombres
#' comunes completos. Ej: "COA" en "Coahuila". Evita nombres extensos como
#' "Coahuila de Zaragoza" y prefiere las apelaciones simples. Elimina caracteres
#' especiales, acentos y puntiacion antes de hacer la transformacion. No ve
#' Mayúsculas ni minúsculas. Si la entidad no se encuentra regresa NA.
#'
#' @param entidad Nombre de una entidad
#' @return un vector de caracteres de tamaño 3 de entidades de México no
#' ambiguas
#' @export
abr_to_entidad <- function(x) {
  y <- stringi::stri_trans_general(x,"latin-ascii" )
  y <- stringi::stri_replace_all(y, regex = "[:punct:]", "")
  y <- stringi::stri_replace_all(y, regex = "[:blank:]", "")

  y <- stringr::str_to_upper(y)

  case_when(
    y == "AGS" ~ "Aguascalientes",
    y == "BJC" ~ "Baja California",
    y == "BC" ~ "Baja California",
    y == "BCS" ~ "Baja California Sur",
    y == "CAM" ~ "Campeche",
    y == "CAMP" ~ "Campeche",
    y == "CHS" ~ "Chiapas",
    y == "CHIS" ~ "Chiapas",
    y == "CHA" ~ "Chihuahua",
    y == "CHIH" ~ "Chihuahua",
    y == "CDM" ~ "Ciudad de México",
    y == "CDMX"~ "Ciudad de México",
    y == "COA" ~ "Coahuila",
    y == "COAH" ~ "Coahuila",
    y == "COL" ~ "Colima",
    y == "DUR" ~ "Durango",
    y == "DGO" ~ "Durango",
    y == "MEX" ~ "México",
    y == "EDOMEX" ~ "Mexico",
    y == "GUA" ~ "Guanajuato",
    y == "GTO" ~ "Guanajuato",
    y == "GUE" ~ "Guerrero",
    y == "GRO" ~ "Guerrero",
    y == "HID" ~ "Hidalgo",
    y == "HGO" ~ "Hidalgo",
    y == "JAL" ~ "Jalisco",
    y == "MIC" ~ "Michoacán",
    y == "MICH" ~ "Michoacán",
    y == "MOR" ~ "Morelia",
    y == "NAY" ~ "Nayarit",
    y == "NOL" ~ "Nuevo León",
    y == "NL" ~ "Nuevo León",
    y == "OAX" ~ "Oaxaca",
    y == "PUE" ~ "Puebla",
    y == "QUE" ~ "Querétaro",
    y == "QRO" ~ "Querétaro",
    y == "QROO" ~ "Quintana Roo",
    y == "QUI" ~ "Quintana Roo",
    y == "SLP" ~ "San Luis Potosí",
    y == "SIN" ~ "Sinaloa",
    y == "SON" ~ "Sonora",
    y == "TAB" ~ "Tabasco",
    y == "TAM" ~ "Tamaulipas",
    y == "TAMPS" ~ "Tamaulipas",
    y == "TLA" ~ "Tlaxcala",
    y == "TLAX" ~ "Tlaxcala",
    y == "VER" ~ "Veracruz",
    y == "YUC" ~ "Yucatán",
    y == "ZAC" ~ "Zacatecas",
    y == "NAC" ~ "Nacional",
    y == "REP" ~ "Nacional",
    T ~ y
  )
}
#' Abrevia una entidad
#'
#' Transforma un nombre propio de entidad Mexicana en abreviaciones no ambiguas.
#' Remueve todo caracter especial o acentuado y luego intenta. Si la entidad no
#' se encuentra regresa NA. Toda referencia a la Republica Federal o Nacion o
#' Nacional se transformará en "NAC". Oaxaca es un caso especial: cualquier
#' mencion de "oaxaca" como match de regex en el nombre se identificará con
#' "OAX".
#'
#' @param entidad Nombre de una entidad
#' @return un vector de caracteres de tamaño 3 de entidades de México no
#' ambiguas
#' @export
entidad_to_abr <- function(entidad) {
  y <- stringi::stri_trans_general(entidad,"latin-ascii" )
  y <- stringi::stri_replace_all(y, regex = "[:punct:]", "")
  y <- stringr::str_to_title(y)

  case_when(
    y == "Tabasco" ~ "TAB",
    y == "Nayarit" ~ "NAY",
    y == "Durango" ~ "DUR",
    stringi::stri_detect(y, fixed ="oaxaca", case_insensitive = T) ~ "OAX",
    y == "Oaxaca" ~ "OAX",
    y == "Mexico" ~ "MEX",
    y == "Edomex" ~ "MEX",
    y == "Estado De Mexico" ~ "MEX",
    y == "Campeche" ~ "CAM",
    y == "Zacatecas" ~ "ZAC",
    y == "Quintana Roo" ~ "QUI",
    y == "Sonora" ~ "SON",
    y == "Cdmx" ~ "CDM",
    y == "Distrito Federal" ~ "CDM",
    y == "Ciudad De Mexico" ~ "CDM",
    y == "Veracruz De Ignacio De La Llave" ~ "VER",
    y == "Veracruz" ~ "VER",
    y == "Baja California Sur" ~ "BCS",
    y == "Morelos" ~ "MOR",
    y == "Guanajuato" ~ "GUA",
    y == "Jalisco" ~ "JAL",
    y == "Tamaulipas" ~ "TAM",
    y == "Guerrero" ~ "GUE",
    y == "Baja California" ~ "BJC",
    y == "Nuevo Leon" ~ "NOL",
    y == "Chihuahua" ~ "CHA",
    y == "San Luis Potosi" ~ "SLP",
    y == "Tlaxcala" ~ "TLA",
    y == "Yucatan" ~ "YUC",
    y == "Puebla" ~ "PUE",
    y == "Coahuila De Zaragoza" ~ "COA",
    y == "Coahuila" ~ "COA",
    y == "Colima" ~ "COL",
    y == "Hidalgo" ~ "HID",
    y == "Queretaro" ~ "QUE",
    y == "Sinaloa" ~ "SIN",
    y == "Chiapas" ~ "CHS",
    y == "Michoacan De Ocampo" ~ "MIC",
    y == "Michoacan" ~ "MIC",
    y == "Morelia"~ "MOR",
    y == "Aguascalientes" ~ "AGS",
    y == "Nacional" ~ "NAC",
    y == "Nacion" ~ "NAC",
    y == "Republica" ~ "NAC",
    y == "Republica Federal" ~ "NAC",
    T ~ y
  )
}
#' @export
`%notin%` <- Negate(`%in%`)
#' @export
`%+%` <- paste0
