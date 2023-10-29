seriePlotIndividual <- function(ciudad, candidato, tablas) {
  lista_tablas[[ciudad]] %>%
    select(-c(Encuestador, `Margen de error`, Fuente)) %>%
    pivot_longer(cols = -Fecha) %>%
    filter(name == candidato) %>%
    ggplot(aes(x = Fecha, y = value)) +
    geom_point(color = "dodgerblue2", size = 1) +
    geom_line(color = "dodgerblue2") +
    geom_smooth(method = "gam",
                formula = y ~ ns(x, df = 3),
                color = "firebrick") +
    labs(y = "Intención de voto (%)") +
    scale_x_date(date_labels = "%b/%Y")
}