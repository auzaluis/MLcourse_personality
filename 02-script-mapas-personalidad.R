
library(FactoMineR)

# ETL ----
DF10 <- DF9 %>%
  
  # Seleccionar solo las variables de interés
  select(Edad.GR, app, time) %>% 
  
  # Agrupar por las variables de interés
  group_by(Edad.GR, app) %>%
  
  # Sumar el tiempo
  summarise(time_sum = sum(time)) %>% 
  
  ungroup() %>% 
  
  # Pivotar la variable app
  pivot_wider(names_from = app,
              values_from = time_sum) %>% 
  
  # Pasar las columna Edad a nombre de fila
  column_to_rownames(var = "Edad.GR")



# Mapa perceptual ----
FactoMineR::CA(DF10)
