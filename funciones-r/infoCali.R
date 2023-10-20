infoCali <- function(url) {
  tabla_cali <-
    url %>%
    read_html() %>%
    html_node("#mw-content-text > div.mw-parser-output > div:nth-child(21) > center > table") %>%
    html_table()
  
  nombres_columnas <-
    tabla_cali[1,] %>%
    t() %>%
    as.vector() %>%
    na.omit() %>%
    c("Fecha", "Encuestador", .)
  
  tabla_final <-
    tabla_cali %>%
    set_names(nombres_columnas) %>%
    slice(-1) %>%
    mutate(
      across(
        c(`Alejandro Eder`:`Margen de error`),
        ~ str_replace_all(., ",", ".")
      ),
      across(
        c(`Alejandro Eder`:`Margen de error`),
        ~ str_replace_all(., "%", "")
      ),
      across(c(`Alejandro Eder`:`Margen de error`), as.numeric),
      Fecha = dmy(Fecha)
    ) %>%
    slice(-1) %>%
    filter(!is.na(`Margen de error`)) %>%
    rename(`NS/NR` = `Indeciso(Ns/Nr)`)
  
  return(tabla_final)
}