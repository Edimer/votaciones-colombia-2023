seriePlot <- function(ciudad, tablas) {
  tablas[[ciudad]] %>%
    select(-c(Encuestador, `Margen de error`, Fuente)) %>%
    pivot_longer(cols = -Fecha) %>%
    ggplot(aes(x = Fecha, y = value)) +
    facet_wrap(~ name, scales = "free_y", ncol = 4) +
    geom_point(color = "dodgerblue2", size = 1) +
    geom_line(color = "dodgerblue2") +
    geom_smooth(method = "gam",
                formula = y ~ ns(x, df = 3),
                color = "firebrick") +
    labs(y = "Intención de voto (%)") +
    scale_x_date(date_labels = "%b/%Y") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}