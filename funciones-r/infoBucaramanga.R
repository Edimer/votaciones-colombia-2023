infoBucaramanga <- function(url) {
  tabla_bcmanga <-
    url %>%
    read_html() %>%
    html_node("#mw-content-text > div.mw-parser-output > center:nth-child(26) > table") %>%
    html_table()
  
  nombres_columnas <-
    tabla_bcmanga[1,] %>%
    t() %>%
    as.vector() %>%
    na.omit()
  
  tabla_final <-
    tabla_bcmanga %>%
    set_names(nombres_columnas) %>%
    slice(-c(1, 2)) %>%
    mutate(
      across(
        c(`Consuelo Ordoñez`:`Margen de error`),
        ~ str_replace_all(., ",", ".")
      ),
      across(
        c(`Consuelo Ordoñez`:`Margen de error`),
        ~ str_replace_all(., "%", "")
      ),
      across(c(`Consuelo Ordoñez`:`Margen de error`), as.numeric),
      Fecha = dmy(Fecha)
    )  %>%
    filter(!is.na(`Margen de error`))
  
  return(tabla_final)
}