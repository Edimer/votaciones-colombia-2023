distPlot <- function(ciudad, tablas) {
  if (ciudad == "bogota") {
    tablas[[ciudad]] %>%
      pivot_longer(cols = c(Oviedo:`NS/NR`)) %>%
      ggplot(aes(x = reorder(name, value, na.rm = TRUE), y = value)) +
      geom_boxplot(color = "dodgerblue2", fill = "dodgerblue2", alpha = 0.4) +
      scale_y_log10() +
      labs(x = "", y = "Intención de voto (%)") +
      coord_flip()
  } else if (ciudad == "medellin") {
    tablas[[ciudad]] %>%
      pivot_longer(cols = c(Aguinaga:`NS/NR`)) %>%
      ggplot(aes(x = reorder(name, value, na.rm = TRUE), y = value)) +
      geom_boxplot(color = "dodgerblue2", fill = "dodgerblue2", alpha = 0.4) +
      scale_y_log10() +
      labs(x = "", y = "Intención de voto (%)") +
      coord_flip()
  } else if (ciudad == "cali") {
    tablas[[ciudad]] %>%
      pivot_longer(cols = c(`Alejandro Eder`:`NS/NR`)) %>%
      ggplot(aes(x = reorder(name, value, na.rm = TRUE), y = value)) +
      geom_boxplot(color = "dodgerblue2", fill = "dodgerblue2", alpha = 0.4) +
      scale_y_log10() +
      labs(x = "", y = "Intención de voto (%)") +
      coord_flip()
  } else if (ciudad == "bucaramanga") {
    tablas[[ciudad]] %>%
      pivot_longer(cols = c(`Consuelo Ordoñez`:`NS/NR`)) %>%
      ggplot(aes(x = reorder(name, value, na.rm = TRUE), y = value)) +
      geom_boxplot(color = "dodgerblue2", fill = "dodgerblue2", alpha = 0.4) +
      scale_y_log10() +
      labs(x = "", y = "Intención de voto (%)") +
      coord_flip()
  } else if (ciudad == "barranquilla") {
    tablas[[ciudad]] %>%
      pivot_longer(cols = c(`Alejandro Char`:`NS/NR`)) %>%
      ggplot(aes(x = reorder(name, value, na.rm = TRUE), y = value)) +
      geom_boxplot(color = "dodgerblue2", fill = "dodgerblue2", alpha = 0.4) +
      scale_y_log10() +
      labs(x = "", y = "Intención de voto (%)") +
      coord_flip()
  } else if (ciudad == "cartagena") {
    tablas[[ciudad]] %>%
      pivot_longer(cols = c(`Dumek Turbay`:`NS/NR`)) %>%
      ggplot(aes(x = reorder(name, value, na.rm = TRUE), y = value)) +
      geom_boxplot(color = "dodgerblue2", fill = "dodgerblue2", alpha = 0.4) +
      scale_y_log10() +
      labs(x = "", y = "Intención de voto (%)") +
      coord_flip()
  }
}