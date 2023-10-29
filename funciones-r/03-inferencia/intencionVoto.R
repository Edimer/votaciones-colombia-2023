intencionVoto <- function(ciudad, tablas, num_reps) {
  candidatos =
    tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, Fuente, `Margen de error`)) %>%
    names()
  
  res1 =
    tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, Fuente, `Margen de error`)) %>%
    map_df(.x = .,
           .f = ~ infer1(x = .x)) %>%
    mutate(Método = "t-student",
           Candidato = candidatos) %>%
    relocate(Candidato, Método, everything())
  
  res2 =
    tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, Fuente, `Margen de error`)) %>%
    map_df(.x = .,
           .f = ~ infer2(x = .x)) %>%
    mutate(Método = "t-student - log",
           Candidato = candidatos) %>%
    relocate(Candidato, Método, everything())
  
  res3 =
    tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, Fuente, `Margen de error`)) %>%
    map_df(.x = .,
           .f = ~ infer3(x = .x, n_reps = num_reps)) %>%
    mutate(Método = "BootPromedio-ICQ",
           Candidato = candidatos) %>%
    relocate(Candidato, Método, everything())
  
  res4 =
    tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, Fuente, `Margen de error`)) %>%
    map_df(.x = .,
           .f = ~ infer4(x = .x, n_reps = num_reps)) %>%
    mutate(Método = "BootPromedio-ICEE",
           Candidato = candidatos) %>%
    relocate(Candidato, Método, everything())
  
  res5 =
    tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, Fuente, `Margen de error`)) %>%
    map_df(.x = .,
           .f = ~ infer5(x = .x, n_reps = num_reps)) %>%
    mutate(Método = "BootMediana-ICQ",
           Candidato = candidatos) %>%
    relocate(Candidato, Método, everything())
  
  res6 =
    tablas[[ciudad]] %>%
    select(-c(Fecha, Encuestador, Fuente, `Margen de error`)) %>%
    map_df(.x = .,
           .f = ~ infer6(x = .x, n_reps = num_reps)) %>%
    mutate(Método = "BootMáximo-ICQ",
           Candidato = candidatos) %>%
    relocate(Candidato, Método, everything())
  
  tabla_estimaciones =
    bind_rows(res1, res2, res3, res4, res5, res6)
  
  return(tabla_estimaciones)
}