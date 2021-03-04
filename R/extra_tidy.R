#' Group by and Ponder
#'
#' Esta función toma una base de datos y la transforma a una muestra de tidy con
#' .variable_a_pond como probabilidades. Funciona dentro de Tidy solamente en el
#' pipe (%>%).
#'
#' Si se quiere utilizar fuera referirse a sym(!!String)
#'
#' @param .tabla data.frame o tabla a ponderar
#' @param .variable_a_pond variable que se va a ponderar en factor o String
#' @param .ponderador variable de probabilidades
#' @param ... variables extra de agrupación tipo group_by anteriores a .variable_a_pond
#'
#' @return Regresa una bd con proporciones ponderadas, por grupo además de
#' observaciones sin ponderar (v_obs) y ponderadas (v_tot)
#' @export
group_and_ponder_by <- function(.tabla, .variable_a_pond, .ponderador, ...){
  variable_a_pond <- enquo(.variable_a_pond)
  ponderador <- enquo(.ponderador)
  group_vars <- enquos(...)
  .tabla%>%
    ungroup() %>%
    filter(across(c(!!!group_vars,!!variable_a_pond), ~!is.na(.))) %>%
    filter(across (starts_with("f_"), ~!stri_detect(., fixed = "no", case_insensitive = T)))  %>%
    as_survey_design(probs = !!ponderador) %>%
    srvyr::group_by(!!!group_vars,!!variable_a_pond, .drop = T) %>%
    summarise(v_tot = survey_total(na.rm = T, vartype = "se", levels = 0.95),
              v_obs = n(),
              v_prop = survey_mean(na.rm = T, vartype = "se", levels = 0.95)) %>%
    mutate(num_pregunta = rlang::as_string(enexpr(.variable_a_pond)),
           fac = rlang::as_string(enexpr(.ponderador))) %>%
    rename(preferencia = !!variable_a_pond) %>%
    select(!!!group_vars,num_pregunta,fac, preferencia, contains(c("v_prop","v_obs", "v_tot")))
}

#' Group by and weight Ponder
#'
#' Esta función toma una base de datos y la transforma a una muestra de tidy con
#' .variable_a_pond como probabilidades. Funciona dentro de Tidy solamente en el
#' pipe (%>%).
#'
#' Si se quiere utilizar fuera referirse a sym(!!String)
#'
#' @param .tabla data.frame o tabla a ponderar
#' @param .variable_a_pond variable que se va a ponderar en factor o String
#' @param .ponderador variable de probabilidades
#' @param ... variables extra de agrupación tipo group_by anteriores a .variable_a_pond
#'
#' @return Regresa una bd con proporciones ponderadas, por grupo además de
#' observaciones sin ponderar (v_obs) y ponderadas (v_tot)
#' @export
group_and_wponder_by <- function(.tabla, .variable_a_pond, .ponderador, ...){
  variable_a_pond <- enquo(.variable_a_pond)
  ponderador <- enquo(.ponderador)
  group_vars <- enquos(...)
  .tabla%>%
    ungroup() %>%
    filter(across(c(!!!group_vars,!!variable_a_pond), ~!is.na(.))) %>%
    filter(across (starts_with("f_"), ~ifelse(is.na(.),T,. == "Sí")))  %>%
    as_survey_design(weights = !!ponderador) %>%
    srvyr::group_by(!!!group_vars,!!variable_a_pond, .drop = T) %>%
    summarise(v_tot = survey_total(na.rm = T, vartype = "se", levels = 0.95),
              v_obs = n(),
              v_prop = survey_mean(na.rm = T, vartype = "se", levels = 0.95)) %>%
    mutate(num_pregunta = rlang::as_string(enexpr(.variable_a_pond)),
           fac = rlang::as_string(enexpr(.ponderador))) %>%
    rename(preferencia = !!variable_a_pond) %>%
    select(!!!group_vars,num_pregunta,fac, preferencia, contains(c("v_prop","v_obs", "v_tot")))
}
