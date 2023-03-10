
# Tema 01: Carga datos ----

## Carga local
DF <- read.csv(file = "Personalidad y uso de apps (respuestas) - Respuestas de formulario.csv",
               check.names = FALSE)

## Carga en línea
install.packages("gsheet")
library(gsheet)

DF <- read.csv(text = gsheet2text(url = "https://docs.google.com/spreadsheets/d/1IQ_RxxTSmBKHTExlxboIRNlMov_F6RyqdcOPrflCv_w/edit?usp=sharing"),
               check.names = F)



# Estructura del data frame ----

class(DF)
nrow(DF)
ncol(DF)

class(DF$`Escribe tu edad exacta`)
class(DF$Sexo)



# Tema 02: Transformación de datos ----

## Valores perdidos ----

DF$`Escribe tu edad exacta`
is.na(DF$`Escribe tu edad exacta`)
summary(is.na(DF$`Escribe tu edad exacta`))

### Reemplazo con la media ----
install.packages("tidyverse")
library(tidyverse)

DF2 <- DF %>%
  mutate(`Escribe tu edad exacta` = ifelse(test = is.na(`Escribe tu edad exacta`),
                                           yes = mean(`Escribe tu edad exacta`, na.rm = T),
                                           no = `Escribe tu edad exacta`))

### Eliminar la fila completa ----

DF2 <- DF %>% na.omit()



## Estandarización de variables ----

### Normalización ----

scale(DF2$`Escribe tu edad exacta`)

# Comparando la variable variable original con la normalizada
data.frame(original = DF2$`Escribe tu edad exacta`,
           normalizado = scale(DF2$`Escribe tu edad exacta`)) %>%
  
  # Muestra los 10 primeros casos
  head(n = 10)



# Agregar una nueva columna al DF2: scale(edad)

DF3 <- DF2 %>%
  # Creando nueva variable
  mutate(Edad.Z = scale(`Escribe tu edad exacta`)) %>%
  
  # Re-ubicando una variable
  relocate(Edad.Z,
           .after = `Escribe tu edad exacta`)



### Convertir a rango ----
library(scales)

rescale(DF2$`Escribe tu edad exacta`)

data.frame(original = DF2$`Escribe tu edad exacta`,
           rescaled = rescale(DF2$`Escribe tu edad exacta`))



## Agrupaciones ----

### Numéricas ----

DF4 <- DF3 %>%
  
  # Crear la variable que agrupa las edades
  mutate(Edad.GR = cut(`Escribe tu edad exacta`,
                       breaks = c(-Inf, 18, 23, Inf),
                       labels = c("18 o menos", "19 a 23", "Más de 23"))) %>%
  
  # Re-ubicando la variable
  relocate(Edad.GR,
           .after = Edad.Z)



### Categóricas ----

unique(DF4$`Según tu forma de ser ¿Cuál de las siguientes frases describe mejor tu forma de ser y pensar? [Me gusta comprar marcas que representen mi status]`)

colnames(DF4)
DF4[,9]

ifelse(test = DF4[,9] == "Un poco verdadero" | DF4[,9] == "Totalmente verdadero",
       yes = 1,
       no = 0)



### (Paréntesis) Bucles ----

#### Paso 1: Crear un vector que contenga los nombres
#### de las variables donde se va a aplicar el bucle
frases <- DF4 %>%
  select(starts_with("Según tu")) %>%
  colnames()

frases

#### Creando un DF por temas pedagógicos
DF5 <- DF4

#### Paso 2: Ejecutar el bucle
for (variable in frases) {
  
  DF5[,variable] <- ifelse(
    test = DF5[,variable] == "Totalmente verdadero" | DF5[,variable] == "Un poco verdadero",
    yes = 1,
    no = 0
  )
  
}



## Manipulación de data frames ----

# Convirtiendo en data.frame a tibble
DF5 <- DF5 %>% as_tibble()

### Función select: columnas ----
DF5 %>% select(Sexo)
DF5 %>% select(Sexo, `Escribe tu edad exacta`)
DF5 %>% select(-`Marca temporal`)
DF5 %>% select(starts_with("¿Cuánto"))
DF5 %>% select(ends_with("]"))
DF5 %>% select(contains("días"))



### Función filter: filas ----
DF5 %>% filter(Sexo == "Mujer")
DF5 %>% filter(Sexo != "Hombre") # Negación
DF5 %>% filter(`Escribe tu edad exacta` >= 20) # Mayor o igual que
DF5 %>% filter(`Escribe tu edad exacta` <= 20) # Menor o igual que
DF5 %>% filter(`Escribe tu edad exacta` >= 18 &
                 `Escribe tu edad exacta` <= 21) # Rango

# Ejercicio: Mujer entre 15 y 18 años
DF5 %>% filter(Sexo == "Mujer",
               `Escribe tu edad exacta` >= 15,
               `Escribe tu edad exacta` <= 18)

# Ejercicio:
# Columnas: Edad y [Me gusta comprar marcas que representen mi status]
# Filas: [Me gusta comprar marcas que representen mi status] igual a 1

DF5 %>%
  select(`Escribe tu edad exacta`,
         `Según tu forma de ser ¿Cuál de las siguientes frases describe mejor tu forma de ser y pensar? [Me gusta comprar marcas que representen mi status]`) %>%
  filter(`Según tu forma de ser ¿Cuál de las siguientes frases describe mejor tu forma de ser y pensar? [Me gusta comprar marcas que representen mi status]` == 1)



### Cambio de nombre de columnas ----
DF6 <- DF5
colnames(DF6)

# APPS
## Paso 1: Creamos un vector con los nuevos nombres
apps <- c("TikTok", "Instagram", "Facebook", "YouTube")

## Paso 2: Reemplazar los nombres antiguos por los del vector creado
colnames(DF6)[33:36] <- apps
colnames(DF6)



# Frases
## Paso 1: Creamos un vector con los nuevos nombres
frases2 <- frases %>%
  as_tibble() %>%
  separate(col = value,
           into = c("No sirve", "Sirve"),
           sep = "\\[") %>%
  select(-"No sirve") %>%
  separate(col = Sirve,
           into = c("Sirve", "No sirve"),
           sep = "\\]") %>%
  select(-"No sirve") %>%
  as_vector()


## Paso 2: Reemplazar los nombres antiguos por los del vector creado
colnames(DF6)[8:31] <- frases2
colnames(DF6)



### Pivotado de base ----

#### Pivot longer

DF7 <- DF6 %>%
  select(`Marca temporal`, Sexo, Edad.GR ,apps) %>%
  pivot_longer(cols = apps,
               names_to = "app",
               values_to = "time")



#### Pivot wider

DF8 <- DF7 %>%
  pivot_wider(names_from = app,
              values_from = time)



## (Paréntesis) Transformación  horas ----

class(DF7$time)
head(DF7$time)

# strsplit separa los textos
strsplit(x = DF7$time,
         split = ":") %>%
  head()

# transformación
DF7$time <- sapply(X = strsplit(x = DF7$time,
                                split = ":"),
                   function(x){
                     
                     x <- as.numeric(x)
                     x[1] + x[2]/60 + x[3]/60^2
                     
                   })



## Detección de outliers ----

### Gráfica: Boxplots ----

# Boxplot feo
boxplot(x = DF7$time)

# Boxplot bonito
install.packages("plotly")
library(plotly)

# ggplotly que hace el gráfico sea interactivo
ggplotly(
  
  # Definir el DF
  DF7 %>%
    
    # Dibujar el lienzo
    ggplot(mapping = aes(x = app,
                         y = time,
                         fill = app)) +
    
    # Especificar el tipo de gráfico
    geom_boxplot() +
    geom_jitter(width = .2,
                alpha = .5) +
    
    # Definir el tema
    theme_minimal() +
    
    # Otras opciones de tema
    theme(legend.position = "none")
  
)



# Mismo gráfico pero normalizado
ggplotly(
  
  DF7 %>%
    
    # Para calcular por grupos
    group_by(app) %>%
    
    mutate(scaled = scale(time)) %>% 
    
    # Una vez hecho el cálculo, desagrupar (pro tip)
    ungroup() %>% 
    
    ggplot(mapping = aes(x = app,
                         y = scaled,
                         fill = app)) +
    
    geom_boxplot() +
    
    labs(x = "",
         y = "Horas a la semana",
         title = "Cantidad de tiempo en redes sociales") +
    
    theme_minimal() +
    
    theme(legend.position = "none")
  
)



### Tratamiento de outliers ----

DF9 <- DF7 %>% 
  group_by(app) %>%
  mutate(scaled = scale(time)) %>% 
  ungroup() %>%
  
  # Reemplazo por NA, es decir, eliminar
  mutate(time_na = ifelse(test = scaled > 2,
                          yes = NA,
                          no = time),
         
         # Reemplazo por la mediana
         time_median = ifelse(test = scaled >2,
                              yes = median(time),
                              no = time))
