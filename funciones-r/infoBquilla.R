infoBquilla <- function(url) {
  tabla_bquilla <-
    url %>%
    read_html() %>%
    html_node("#mw-content-text > div.mw-parser-output > center:nth-child(25) > table") %>%
    html_table()
  
  nombres_columnas <-
    tabla_bquilla[1, ] %>%
    t() %>%
    as.vector() %>%
    na.omit()
  
  tabla_final <-
    tabla_bquilla %>%
    set_names(nombres_columnas) %>%
    slice(-c(1, 2)) %>%
    mutate(
      across(
        c(`Alejandro Char`:`Margen de error`),
        ~ str_replace_all(., ",", ".")
      ),
      across(
        c(`Alejandro Char`:`Margen de error`),
        ~ str_replace_all(., "%", "")
      ),
      across(c(`Alejandro Char`:`Margen de error`), as.numeric),
      Fecha = dmy(Fecha)
    ) %>%
    filter(!is.na(`Margen de error`))
  
  return(tabla_final)
}