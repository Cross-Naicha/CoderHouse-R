# Cargando los datos
csv_file <- "metabolicSyndrome Daruich -data.csv"
df <- read.csv(csv_file, sep=",", header = TRUE, stringsAsFactors = TRUE)

# Estructura de los datos
str(df)

# Sumarizando
summary(df)

# Probando la función de Varianza
variance <- function(x) {
  n <- length(x)
  mean <- mean(x, na.rm=TRUE)
  sum_squares <- sum((x-mean)^2)
  
  return(sum_squares/(n-1))
}

# Probando la función de la desviación estándar
sd <- function(x) {
  return(sqrt(variance(x)))
}

# Calculando la varianza y la desviación estándar de la edad
variance(df$Age)
sd(df$Age)

# Integrando en la función Master
master <- function(x) {
  var = variance(x)
  sd = sd(x)
  return(c(var, sd))
}

# Calculando master para edad
master(df$Age)

# Instalando la librería tidyverse
# install.packages("tidyverse"): LISTO

library(tidyverse)

# Probando la función SELECT
select(df,BMI)
select(df,c(BMI,Sex))
view(select(df,c(BMI,Sex,Age)))

# Probando la función MUTATE (+ Actualizando el DF)
df <- mutate(
  df, 
  Weight = ifelse(df$BMI > 35, "Morbid Obesity",
                  ifelse(df$BMI > 30, "Obesity", 
                  ifelse(df$BMI > 25, "Overweight", "Normal"
                  )))
)

select(df, c(BMI, Weight))

df <- mutate(
  df,
  Weight = as.factor(Weight),
  Albuminuria = as.factor(Albuminuria)
)

# Probando la funcion GLIMPSE
glimpse(df)

# Probando la funcion TRANSMUTE
company <- transmute(
  df,
  Marital = as.factor(df$Marital),
  Alone = as.factor(ifelse(Marital == 'Married','No','Yes'))
)

glimpse(company)

# Probando la funcion RENAME
df <- rename(
  df,
  ID = seqn,
  Edad = Age,
  Sexo = Sex,
  Civil = Marital,
  Ingresos = Income,
  Raza = Race,
  "Circ. Abd." = WaistCirc,
  IMC = BMI,
  "Alb/Cr" = UrAlbCr,
  Uricemia = UricAcid,
  Glucemia = BloodGlucose,
  TGL = Triglycerides,
  "Sind. Metabolico" = MetabolicSyndrome,
  Peso = Weight
)

# Probando la funcion FILTER
filter(
  df,
  Glucemia > 126
)

# Retornando a la función MUTATE (cambios necesarios)
df <- mutate(
  df,
  "Circ. Abd. +" = ifelse(((Sexo == "Male" & `Circ. Abd.` > 102) | (Sexo == "Female" & `Circ. Abd.` > 88)),1,0),
  Hipertrigliceridemia = ifelse(TGL > 150, 1, 0),
  Dislipidemia = ifelse(((Sexo == "Male" & HDL < 40) | (Sexo == "Female" & HDL < 50)),1 ,0),
  Hiperglucemia = ifelse(Glucemia > 110, 1, 0),
  Solitario = ifelse(Civil != 'Married', 1, 0)
)

# Retornando a la función FILTER
filter(
  df,
  Hiperglucemia == 1,
  Hipertrigliceridemia == 1,
  Dislipidemia == 1
)

# Probando la función GROUP BY
df %>% group_by(Raza) %>% summarise(glucemia_promedio = mean(Glucemia)) %>% arrange(glucemia_promedio)
df %>% group_by(Solitario) %>% summarise(count_Hiperglucemia = sum(Hiperglucemia == 1))

unique(df$Civil) 
