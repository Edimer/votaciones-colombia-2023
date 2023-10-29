corPlot <- function(ciudad, tablas) {
  tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, `Margen de error`, Fuente)) %>%
    correlate(method = "spearman") %>%
    mutate(across(everything(), ~ replace_na(., 0))) %>%
    network_plot(colors = c("firebrick", "dodgerblue2"))
}