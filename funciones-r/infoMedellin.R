infoMedellin <- function(url) {
  tabla_medellin <-
    url %>%
    read_html() %>%
    html_node("#mw-content-text > div.mw-parser-output > div:nth-child(43) > center > table") %>%
    html_table()
  
  nombres_columnas <-
    tabla_medellin[1, ] %>%
    t() %>%
    as.vector() %>%
    na.omit() %>%
    c("Fecha", "Encuestador", .)
  
  tabla_final <-
    tabla_medellin %>%
    set_names(nombres_columnas) %>%
    select(-Vélez) %>%
    slice(-1) %>%
    mutate(
      across(c(Aguinaga:`Margen de error`), ~ str_replace_all(., ",", ".")),
      across(c(Aguinaga:`Margen de error`), ~ str_replace_all(., "%", "")),
      across(c(Aguinaga:`Margen de error`), as.numeric),
      Fecha = dmy(Fecha)
    ) %>%
    slice(-1) %>%
    filter(!is.na(`Margen de error`))
  
  return(tabla_final)
}