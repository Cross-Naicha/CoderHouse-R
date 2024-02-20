# Cargando la librería
library(dplyr)
library(janitor)
library(tidyverse)
library(ggplot2)
library(tidymodels)
library(factoextra)
library(cluster)

# Cargando los datos
csv_file <- "metabolicSyndrome Daruich -data.csv"
df <- read.csv(csv_file, sep=",", header = TRUE, stringsAsFactors = TRUE)

# Información Estructural Pre-Wrangling
estructura <- function(dataframe) {
  num_filas <- nrow(dataframe)
  num_colum <- ncol(dataframe)
  nan <- sum(is.na(dataframe))
  
  print(paste("El dataframe tiene", num_filas, "filas, y", num_colum, "columnas.", "Valores NA =", nan))
}

estructura(df)

# Reemplazando la columna "seqn" por un Indice
df <- select(df, -seqn, -UrAlbCr) %>% 
  mutate(Index = row_number()) %>%
  clean_names()

# Adecuando los nombres de todas las columnas
df <- rename(
  df,
  edad = age,
  sexo = sex,
  civil = marital,
  ingresos = income,
  raza = race,
  abdomen = waist_circ,
  imc = bmi,
  uricemia = uric_acid,
  glucemia = blood_glucose,
  trigliceridemia = triglycerides,
  metabolico = metabolic_syndrome
)

# Codificando los valores de las variables
df <- mutate(
  df,
  sexo = recode(sexo, "Female" = 0, "Male" = 1),
  civil = recode(civil, "Unknow" = 0, "Married" = 1, "Widowed" = 2, "Divorced" = 3, "Separated" = 4, "Single" = 5),
  raza = recode(raza, "White" = 1, "Asian" = 2, "Black" = 3, "MexAmerican" = 4, "Hispanic" = 5, "Other" = 6)
  )

# Creando columnas de criterios especificos
df <- mutate(
  df,
  central = ifelse(((sexo == 1 & abdomen > 102) | (sexo == 0 & abdomen > 88)),1,0),
  etareo = ifelse(df$edad > 70, 6,
           ifelse(df$edad > 60, 5,
           ifelse(df$edad > 50, 4,
           ifelse(df$edad > 40, 3,
           ifelse(df$edad > 30, 2, 1))))),
  peso = ifelse(df$imc > 35, 5,
           ifelse(df$imc > 30, 4,
           ifelse(df$imc > 25, 3,
           ifelse(df$imc > 18.5, 2,1)))),
  hipertrigliceridemia = ifelse(trigliceridemia > 150, 1, 0),
  dislipidemia = ifelse(((sexo == 1 & hdl < 40) | (sexo == 0 & hdl < 50)),1 ,0),
  hiperglucemia = ifelse(glucemia > 100, 1, 0),
  hiperuricemia = ifelse(((sexo == 0 & uricemia > 6) | (sexo == 1 & uricemia > 7)),1 ,0)
)

# Reordenando las columnas
df <- select(
  df, index, sexo, raza, edad, etareo, civil, ingresos, abdomen, central, imc, peso, glucemia, hiperglucemia,
  trigliceridemia, hipertrigliceridemia, hdl, dislipidemia, uricemia, hiperuricemia, albuminuria, metabolico
)

# Transformando en factor (cuando corresponde)
df <- mutate(
  df,
  sexo = as.factor(sexo),
  etareo = as.factor(etareo),
  raza = as.factor(raza),
  civil = as.factor(civil),
  central = as.factor(central),
  peso = as.factor(peso),
  hiperglucemia = as.factor(hiperglucemia),
  hipertrigliceridemia = as.factor(hipertrigliceridemia),
  dislipidemia = as.factor(dislipidemia),
  hiperuricemia = as.factor(hiperuricemia),
  albuminuria = as.factor(albuminuria),
  metabolico = as.factor(metabolico)
)

# TODO Crear una función que resuma la creación de tablas de referencia
# Creando tabla de referencia: edad
grupos <- c(1,2,3,4,5,6)
etiquetas <- c("20 - 30", "30 - 40", "40 - 50", "50 - 60", "60 - 70", "70 - 80")

grupos <- as.factor(grupos)

edad_ref <- data.frame(etareo = grupos, etiqueta_edad = etiquetas)

# Creando tabla de referencia: sexo
grupos <- c(0,1)
etiquetas <- c("Mujer", "Hombre")

grupos <- as.factor(grupos)

sexo_ref <- data.frame(sexo = grupos, etiqueta_sexo = etiquetas)

# Creando tabla de referencia: síndrome metabólico
grupos <- c(0,1)
etiquetas <- c("Ausente", "Presente")

grupos <- as.factor(grupos)

metabolico_ref <- data.frame(metabolico = grupos, etiqueta_metabolico = etiquetas)

# Creando tabla de referencia: raza
grupos <- c(1,2,3,4,5,6)
etiquetas <- c("Caucasico", "Asiatico", "Africano", "Mex-Americano", "Hispano", "Otro")

grupos <- as.factor(grupos)

raza_ref <- data.frame(raza = grupos, etiqueta_raza = etiquetas)

# Tratamiento de valores faltantes
df_procesada <- na.omit(df)

# Tratamiento de valores extremos
remove_outliers <- function(column) {
  Q1 <- quantile(column, 0.25)
  Q3 <- quantile(column, 0.75)
  IQR_value <- IQR(column)
  
  outliers <- column < (Q1 - 1.5 * IQR_value) | column > (Q3 + 1.5 * IQR_value)
  
  return(outliers)
}

# Pre-procesamiento Final 
columnas <- c("abdomen", "imc", "glucemia", "trigliceridemia", "hdl", "uricemia")
outliers <- apply(df_procesada[columnas], 2, remove_outliers)
rows_to_remove <- apply(outliers, 1, any)
df_procesada <- df_procesada[!rows_to_remove, ]

# Información Estructural Post-Wrangling

estructura(df_procesada)

# Resumen de la información
df_procesada %>% group_by(metabolico) %>% summarise("Mujer" = sum(sexo == 0),
                                                    "Varon" = sum(sexo == 1),
                                                    "Total" = n(),
                                                    "CAB-P" = mean(abdomen),
                                                    "GLU-P" = mean(glucemia),
                                                    "TGL-P" = mean(trigliceridemia),
                                                    "HDL-P" = mean(hdl),
                                                     )

# La edad se relaciona con la posibilidad de síndrome metabólico?
edad_metabolico <- df_procesada %>% group_by(etareo) %>% 
  summarise(positivos = sum(metabolico == 1) / n(), negativos = sum(metabolico == 0) / n())

ggplot(data = edad_metabolico %>% inner_join(edad_ref)) +
  geom_col(mapping = aes(x = etareo, y = positivos, fill = etiqueta_edad)) +
    labs(title = "Edad & Sindrome Metabolico", x = "Grupo Etareo", y = "% Positivos") +
        guides(fill = guide_legend(title = "Grupo Etareo"))

# Alguno de los 2 sexos es determinante a la hora de padecer síndrome metabólico?
sexo_metabolico <- df_procesada %>% 
  inner_join(sexo_ref) %>%
    group_by(etiqueta_sexo) %>% summarise(positivo = sum(metabolico == 1),
                                                  negativo = sum(metabolico == 0),
                                                    total = n(),
                                                      positivo_tasa = positivo / total,
                                                        negativo_tasa = negativo / total)

ggplot(data = df_procesada %>% inner_join(sexo_ref) %>% inner_join(metabolico_ref)) +
  geom_bar(mapping = aes(x = etiqueta_sexo, fill = etiqueta_metabolico), position = "fill") +
    labs(title = "S. Metabolico, Edad y Sexo", x = "Grupo Etareo", y = "Frecuencia") +
        guides(fill = guide_legend(title = "S. Metabolico"))

# Y si ademas se tiene en cuenta la edad?
ggplot(data = df_procesada %>% inner_join(edad_ref) %>% inner_join(metabolico_ref)) +
  geom_bar(mapping = aes(x = etiqueta_edad, fill = etiqueta_metabolico), position = "fill") +
    labs(title = "S. Metabolico & Edad", x = "Grupo Etareo", y = "Porcentaje %") +
      guides(fill = guide_legend(title = "S. Metabolico"))

# Que ocurre en el caso de la etnia?
prevalencia_raza <- df_procesada %>% group_by(raza) %>% summarise("20-30" = sum(etareo == 1) / n(),
                                                                  "30-40" = sum(etareo == 2) / n(),
                                                                  "40-50" = sum(etareo == 3) / n(),
                                                                  "50-60" = sum(etareo == 4) / n(),
                                                                  "60-70" = sum(etareo == 5) / n(),
                                                                  "70-80" = sum(etareo == 6) / n())

prevalencia_raza_long <- pivot_longer(prevalencia_raza, cols = `20-30`:`70-80`, names_to = "Grupo Etareo", values_to = "Proporcion")

ggplot(data = prevalencia_raza_long %>% inner_join(raza_ref), aes(x = as.factor(`Grupo Etareo`), y = Proporcion, group = raza, color = factor(etiqueta_raza))) +  
  geom_line() + 
    geom_point() +
      labs(title = "Prevalencia por Raza y Edad", x = "Grupo Etareo", y = "Porcentaje %") +
        guides(color = guide_legend(title = "Raza"))

# El aumento de la grasa visceral abdominal se relaciona con hipertrigliceridemia?
ggplot(data = df_procesada) +
  geom_point(mapping = aes(x = trigliceridemia, y = abdomen, color = metabolico)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, mapping = aes(x = trigliceridemia, y = abdomen)) +
      labs(title = "Grasa Visceral Abdominal & Trigliceridemia", x = "Trigliceridemia", y = "Circunferencia Abdominal") +
        scale_color_manual(values = c("0" = "pink", "1" = "purple"), labels = c("Ausente", "Presente")) +
          guides(color = guide_legend(title = "S. Metabolico"))

# La hipertrigliceridemia se relaciona con hiperglucemia?
ggplot(data = df_procesada) +
  geom_point(mapping = aes(x = glucemia, y = trigliceridemia, color = metabolico)) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, mapping = aes(x = glucemia, y = trigliceridemia)) +
      labs(title = "Glucemia & Hipertrigliceridemia", x = "Glucemia [mg/dL]", y = "Trigliceridemia [mg/dL]") +
        scale_color_manual(values = c("0" = "darkcyan", "1" = "darkblue"), labels = c("Ausente", "Presente")) +
          guides(color = guide_legend(title = "S. Metabolico"))

### Clusters ###

## Pre-procesamiento
# Separando el dataframe en 3 subetapas: Variables No-Modificables, Modificables (cuantitativas) y Modificables (cualitativas)

# Variables No-Modificables (Aquellas sobre las cuales los individuos no tienen influencia)
# (Sexo, Edad/Etáreo, Raza)
sub_df_inmodificable <- df_procesada %>%
  select(index, sexo, edad, etareo, raza)

# Codificación de Variables Categóricas: Sexo
sub_df_inmodificable_encoded_sexos <- model.matrix(~ index + sexo - 1, sub_df_inmodificable) %>%
  as.data.frame() 

colnames(sub_df_inmodificable_encoded_sexos) <- c("index", "mujer", "hombre")

# Codificación de Variables Categóricas: Raza
sub_df_inmodificable_encoded_razas <- model.matrix(~ index + raza - 1, sub_df_inmodificable) %>%
  as.data.frame() 

colnames(sub_df_inmodificable_encoded_razas) <- c("index", "caucasico", "asiatico", "africano", "mex-americano", "hispano", "otro")

# Combinando las consultas tras la codificación
sub_df_inmodificable_encoded <- inner_join(sub_df_inmodificable_encoded_sexos, sub_df_inmodificable_encoded_razas, by = "index") %>%
  inner_join(sub_df_inmodificable %>% select(index, edad)) %>% as.data.frame()

# Variables Modificables (Cuantitativas) + Escalado de las variables
sub_df_mod_cuan <- df_procesada %>%
  select(abdomen, imc, glucemia, trigliceridemia, hdl, uricemia) %>%
    scale() %>% as.data.frame()
# Agregando la columna indice
sub_df_mod_cuan$index <- seq(nrow(sub_df_mod_cuan))

# Variables Modificables, Cualitativas
sub_df_mod_cual <- df_procesada %>%
  select(index, central, peso, hiperglucemia, hipertrigliceridemia, dislipidemia, hiperuricemia)

# Primera Aproximación: Solo VAR Modificables Cuantitativas

# Visualización bivariada (glucemia + trigliceridemia) con la etiqueta de clasificación incluida
ggplot(df_procesada, aes(x = glucemia, y = trigliceridemia, colour = metabolico)) +
  geom_point() +
    labs(title = "Glucemia & Trigliceridemia", x = "Glucemia [mg/dL]", y = "Trigliceridemia [mg/dL]") +
      scale_color_manual(values = c("0" = "darkcyan", "1" = "darkblue"), labels = c("Ausente", "Presente")) +
        guides(color = guide_legend(title = "S. Metabolico"))

# Combinando las consultas de interés: No-Modificables + Modificables Continuas
df_mod_plus_continual <- inner_join(sub_df_inmodificable_encoded, sub_df_mod_cuan, by = "index")

# Combinando las consultas de interés: No-Modificables + Modificables Continuas
df_mod_plus_categorical <- inner_join(sub_df_inmodificable_encoded, sub_df_mod_cual, by = "index")

# Iterando para encontrar el numero de clusteres ideal (métricas de error mínimas)
kclusts <- tibble(k = 2:5) %>%
  mutate(
    kclust = map(k, ~ kmeans(df_mod_plus_continual, .x)),
    tidied = map(kclust, tidy),
    glanced = map(kclust, glance),
    augmented = map(kclust, augment, df_mod_plus_continual)
  )

# Desanidando los resultados: clusters
clusters <- kclusts %>%
  unnest(cols = c(tidied)) %>%
  select(-c(glanced, augmented, kclust))

# Desanidando los resultados: predicciones
predicciones <- kclusts %>%
  unnest(cols = c(augmented)) %>%
  select(k, abdomen, imc, glucemia, trigliceridemia, hdl, uricemia, .cluster)

# Desanidando los resultados: resumen
resumen <- kclusts %>%
  unnest(cols = c(glanced)) %>%
  select(-c(kclust, tidied, augmented))

# Graficando las posibilidades
ggplot(predicciones, aes(x = glucemia, y = trigliceridemia)) +
  geom_point(aes(color = .cluster), alpha = 0.8) +
    geom_point(data = clusters, size = 5, shape = "+") +
      facet_wrap(~ k) + 
        theme_bw()

# Graficando la métrica WithinSS a valores crecientes de K
ggplot(resumen, aes(x = k, y = tot.withinss)) +
  scale_x_discrete(name = "k", limits = c(2:9)) +
    geom_point() +
      geom_line () +
        theme_bw()

# Generando una matriz de distancia
sub_df_mod_cuan_dist <- sub_df_mod_cuan %>% # Se crea la matriz de distancia sobre el df estandarizado
  dist()

# Logrando el modelo de clusterizacion jerárquica
hc <- hclust(sub_df_mod_cuan_dist, method = "complete") # El primer argumento es la matriz de distancia

# Visualización gráfica: dendograma
plot(hc)
rect.hclust(hc, k = 2, border = 3:4) # (modelo, clusters, colores)

# "Cortando" el árbol para facilitar su visualización
clusters <- cutree(hc, k = 2) # Dado que son 2 los grupos objetivo (positivo y negativo para SM)
cj_results <- table(clusters, df_procesada$metabolico) # Comparando los grupos resultantes: dendograma vs. datos originales

cj_results_df <- as.data.frame(cj_results)

cj_results_df$Freq[[1]]
