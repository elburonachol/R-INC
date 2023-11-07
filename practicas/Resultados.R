# Encuentro 6
# Resultados: operaciones, estadística descriptiva y exportación 

# activamos tidyverse

library(tidyverse)

# leemos datos

guess_encoding("practicas/demoras.csv")

datos <- read_csv2("practicas/demoras.csv")

# exploración rápida

glimpse(datos)

# tenemos 8 variables y 1000 observaciones
# Vamos a trabajar con las variables demora1, 2 y 3

# estas variables representan los intervalos medidos en días
# según:

# demora1: días entre fecha de inicio de síntomas y fecha de 
# primera consulta

# demora2: días entre fecha de primera consulta y fecha de 
# diagnóstico

# demora3: días entre fecha de diagnóstico y fecha de inicio 
# de tratamiento

# función across()

# si quisieramos calcular la media de estas variables 
# podríamos hacer así:

datos |> 
  summarise(media_demora1 = mean(demora1),
            media_demora2 = mean(demora2),
            media_demora3 = mean(demora3))

# como en demora1 tenemos valores NA tenenos que agregar
# el argumento na.rm = T

datos |> 
  summarise(media_demora1 = mean(demora1, na.rm = T),
            media_demora2 = mean(demora2),
            media_demora3 = mean(demora3))

# observemos que se repite la misma estructura por cada una
# de ellas y la idea de la codificación es tratar de evitar 
# las reiteraciones

# el paquete dplyr propone entonces el uso de across()

datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = mean))

# tenemos el mismo problema de los NA, entonces debemos
# agregar el argumento dentro del esquema del across
# esto se hace de la siguiente forma:

datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = ~ mean(.x, na.rm = T)))

# también podemos pedir varios resúmenes simultáneos

datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = list(media = ~ mean(.x, na.rm = T),
                               min = ~ min(.x, na.rm = T),
                               max = ~ max(.x, na.rm = T))))


# de esta forma estamos realizando operaciones masivas bajo
# una misma estructura

# también es muy útil cuando tenemos muchas variables que queremos 
# convertir en un solo paso


# función rowwise()

# así como la gunción group_by() no hace nada sola

datos |> 
  group_by(IDPTE)

# observemos que solo agrega un metadato de agrupamiento
# al dataframe que será respetado cuando combinemos el
# código con un summarise() por ejemplo

# la función rowwise() hace lo mismo

datos |> 
  rowwise()

# veamos que el metadato en este caso dice rowwise

# si necetiamos calcular una nueva variable que sea
# la sumatoria de días de las tres demoras podemos hacer:

datos |> 
    mutate(sumatoria = demora1 + demora2 + demora3)

# tenemos el problema de la segundo observacion que tiene 
# un NA en demora1, la suma devuelve NA

# rowwise viene a posibilitar aplicar funciones que solo
# funcionan naturalmente en forma vertical o por columna, 
# por fila 

# por ejemplo la función sum()

datos |> 
  rowwise() |> 
  mutate(sumatoria = sum(c(demora1, demora2, demora3), na.rm = T))

# aquí estamos sumando a las tres demoras con esa función sum()
# a partir de indicar que la tabla trabaja por fila (rowwise)

# al poder usar na.rm = T evitamos que los resultados de observaciones
# donde alguna de las variables participantes tenga NA sean NA


# función c_across()

# esta función es igual a across pero para filas

datos |> 
  rowwise() |> 
  mutate(sumatoria = sum(c_across(demora1:demora3), na.rm = T))

# se usa un rango o bien cualquier selección de variables

datos |> 
  rowwise() |> 
  mutate(sumatoria = sum(c_across(starts_with("demora")), na.rm = T))

# paquete rstatix

# el paquete rstatix tiene un conjunto de funciones compatibles
# con las tuberías y la filosofia de trabajo de tidyverse

# muchas funciones vienen a reemplazar funciones de R base que
# no son compatibles

# los apartados de funciones de inferencia y post-hoc de modelos
# no será vistas pero avisamos que existen dentro de este paquete


library(rstatix)

# get_summary_stats()

# esta función descriptiva trabaja sobre variables numéricas
# de nuestro dataframe, como por ejemplo las demoras

# el argumento type permite que definamos el conjunto de 
# medidas resumen que queremos visualizar

datos |> 
  select(starts_with("demora")) |> 
  get_summary_stats(type = "common")

# medidas robustas

datos |> 
  select(starts_with("demora")) |> 
  get_summary_stats(type = "robust")

# todas las medidas

datos |> 
  select(starts_with("demora")) |> 
  get_summary_stats(type = "full")

# en todos los casos se omiten los valores NA de las variables


# get_mode()

# una medida ausente entre las funciones comunes del lenguaje 
# es la moda de una variable, es decir el valor que más se repite
# la función get_mode() de rstatix lo hace

datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = get_mode))

# aquí implementada mediante across

# como los ceros son los números más repetidos los podemos
# obviar

datos |> 
  filter(demora1 > 0, demora2 > 0, demora3 > 0) |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = get_mode))


# función freq_table()

# para las variables categóricas tenemos freq_table() 
# que realiza tablas de frecuencia automáticas

# por ejemplo, sobre la variable sexo del paciente

datos |> 
  freq_table(PTESXN)

# paquete gtsummary

# el paquete gtsummary 

library(gtsummary)

# tabla univariada de todo el dataframe

datos |> 
  tbl_summary()
    
# tabla univariada de las variables de interes

datos |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary()

# agregamos mayor especificidad sobre las medidas
# y etiquetas

datos |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  label = PTESXN ~ "Sexo")

# tabla estratificada por sexo 

datos |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(by = PTESXN) |> 
  modify_header(label ~ "**Variable**") |> 
  bold_labels()

# Etiquetado - labelled

library(labelled)

datos_etiquetados <- datos |> 
  set_variable_labels(PTESXN = "Sexo", 
                      demora1 = "Demora Sintomas-1raConsulta", 
                      demora2 = "Demora 1raConsulta-Diagnostico", 
                      demora3 = "Demora Diagnostico-Tratamiento") 

datos_etiquetados |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(by = PTESXN) |> 
  modify_header(label ~ "**Variable**") |> 
  bold_labels()

look_for(datos_etiquetados)

datos_etiquetados <- datos_etiquetados |> 
  set_value_labels(PTESXN = c(Varon = "Hombre", Mujer = "Mujer")) 


look_for(datos_etiquetados)


# Exportar con flextable

frec_sexo <- datos |> 
  freq_table(PTESXN) |> 
  flextable()

resumen_demora <- datos_etiquetados |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(by = PTESXN) |> 
  modify_header(label ~ "**Variable**") |> 
  bold_labels() |> 
  as_flex_table()

save_as_docx("tabla 1" = frec_sexo, 
             "tabla 2" = resumen_demora, 
             path = "practicas/tablas.docx")






