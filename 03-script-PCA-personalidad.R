library(ggcorrplot)
library(FactoMineR)

# En este tema vamos a hacer uso del DF6 como base
# Paso previo: Correlaciones ----

## Convirtiendo frases2 en vector
frases3 <- as.vector(frases2)

## Matriz de correlaciones ----
r <- cor(x = DF6 %>% select(all_of(frases3)),
         method = "spearman")


## Gráfico de matriz de correlaciones

ggplotly(
  
  ggcorrplot(corr = r,
             type = "upper",
             show.legend = F,
             colors = c("red", "white", "blue"),
             tl.cex = 14) +
    
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.grid.major.x = element_blank())
  
)


# PCA: Principal Components Analysis ----

## Dimensión: Extroversión ----

# Definiendo el vector con las frases
extroversion <- frases3[c(8,20,23)]

# Creando la dimensión
PCA.extroversion <- FactoMineR::PCA(X = DF6 %>%
                                      select(all_of(extroversion)),
                                    ncp = 1)

# Eigenvalues y varianza explicada
PCA.extroversion$eig

















