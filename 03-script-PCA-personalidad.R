
# En este tema vamos a hacer uso del DF6 como base

# Paso previo: Correlaciones ----

## Convirtiendo frases2 en vector
frases3 <- as.vector(frases2)

## Matriz de correlaciones ----
r <- cor(x = DF6 %>% select(all_of(frases3)),
         method = "spearman")
View(r)
