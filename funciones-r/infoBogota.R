infoBogota <- function(url) {
  tabla_bogota <-
    url %>%
    read_html() %>%
    html_node("#mw-content-text > div.mw-parser-output > div:nth-child(48)") %>%
    html_table()
  
  nombres_columnas <-
    tabla_bogota[1,] %>%
    t() %>%
    as.vector() %>%
    na.omit() %>%
    c("Fecha", "Encuestador", .)
  
  tabla_final <-
    tabla_bogota %>%
    set_names(nombres_columnas) %>%
    slice(-1) %>%
    mutate(
      across(c(Oviedo:`Margen de error`), ~ str_replace_all(., ",", ".")),
      across(c(Oviedo:`Margen de error`), ~ str_replace_all(., "%", "")),
      across(c(Oviedo:`Margen de error`), as.numeric),
      Fecha = dmy(Fecha)
    ) %>%
    slice(-1)
  
  return(tabla_final)
}