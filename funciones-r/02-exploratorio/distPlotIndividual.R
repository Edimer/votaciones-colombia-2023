distPlotIndividual <-
  function(ciudad, candidato, tablas)  {
    media_candidato =
      tablas[[ciudad]] %>%
      pull(!!sym(candidato)) %>%
      mean(na.rm = TRUE) %>% 
      round(digits = 2)
    
    mediana_candidato =
      tablas[[ciudad]] %>%
      pull(!!sym(candidato)) %>%
      median(na.rm = TRUE) %>% 
      round(digits = 2)
    
    valorp_normalidad =
      tablas[[ciudad]] %>%
      pull(!!sym(candidato)) %>%
      shapiro.test() %>%
      tidy() %>%
      pull(p.value) %>%
      round(digits = 5) %>%
      str_c("Valor p - Shapiro-Wilk = ", .)
    
    res =
      tablas[[ciudad]] %>%
      ggplot(aes(x = !!sym(candidato))) +
      geom_density(
        fill = "dodgerblue2",
        color = "dodgerblue2",
        alpha = 0.3
      ) +
      geom_vline(xintercept = media_candidato,
                 color = "red",
                 lty = 2) +
      geom_vline(xintercept = mediana_candidato,
                 color = "black",
                 lty = 2) +
      labs(
        title = "Distribución de intención de voto",
        subtitle = valorp_normalidad,
        X = candidato,
        y = "Densidad",
        caption = glue(
          "Línea roja: promedio ({media_candidato}%)\nLínea negra: mediana ({mediana_candidato}%"
        )
      ) +
      scale_x_log10()
    
    return(res)
    
  }