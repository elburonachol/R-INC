# Gestión de tablas de datos - Encuentro 5

# activamos tidyverse

library(tidyverse)

# leemos datos

rita_sin_tratamiento <- read_csv2("practicas/rita_sin_tratamiento.csv")

glimpse(rita_sin_tratamiento)

tratamientos <- read_csv2("practicas/tratamientos.csv")

glimpse(tratamientos)

# claves primarias de cada tabla

rita_sin_tratamiento |> 
  count(IDPTE, IDTUM) |> 
  filter(n > 1)

# existen claves duplicadas en la primera tabla

tratamientos |> 
  count(IDTUM, IDTTO) |> 
  filter(n > 1)

# existen claves duplicadas en la segunda tabla

## Detección de duplicados con get_dupes() de janitor

library(janitor)

# duplicados completos 

get_dupes(rita_sin_tratamiento)

# solución posible: observaciones únicas

rita_sin_tratamiento <- rita_sin_tratamiento |> 
  distinct()

# revisamos

get_dupes(rita_sin_tratamiento)

rita_sin_tratamiento |> 
  filter(IDPTE == 1490, IDTUM == 542)

# hacemos lo mismo con la tabla tratamientos

get_dupes(tratamientos)

# no existen observaciones duplicadas completas

get_dupes(tratamientos, IDTUM, IDTTO)

# existen campos claves (clave primaria) duplicados con otros datos diferentes

# solución posible: recorte de observaciones con slice

tratamientos <- tratamientos |> 
  group_by(IDTUM, IDTTO) |>      # agrupamos por clave primaria
  slice_min(FITTO) |>            # recortamos valor mínimo de FIITO
  ungroup()                      # desagrupamos para que no nos quede agrupada
                                 # tratamientos


# verificamos

get_dupes(tratamientos, IDTUM, IDTTO)

tratamientos |> 
  filter(IDTUM == 337, IDTTO == 552)

# habiendo solucionado las duplicaciones y garantizando unicidad en las claves
# procedemos a unir la tablas por IDTUM

rita_completo <- rita_sin_tratamiento |> 
  inner_join(tratamientos, by = "IDTUM") 

# controlamos duplicados con los tres índices

get_dupes(rita_completo, IDPTE, IDTUM, IDTTO)

# Gráfico Upset 

# es un gráfico que permite análizar la frecuencia de diferentes combinaciones 
# de valores/respuestas, por ejemplo cuando cada respuesta puede ser una
# combinación de valores como en el caso de síntomas o comorbilidades.

# en este ejemplo de tratamientos tenemos una variable como ESTTON
# que contiene las diferentes estrategias para cada paciente/tumor, es decir
# que cada unidad de observación puede haber pasado por estrategias distintas
# y queremos cuantificar la frecuencia de sus combinaciones.

# el paquete UpsetR propone a la función upset() para este trabajo

library(UpSetR)

# veamos su documentación

help(upset) 

# si se investiga lo suficiente encontraremos que necesitamos que el conjunto 
# de datos tenga una disposición a lo ancho donde cada columna/categoría de 
# ESTTON contenga valores 0 y 1 (que significa 1 = si, 0 = no) 

rita_completo |> 
  count(ESTTON)

# la variable (como corresponde a datos ordenados) tiene estas categorías

# necesitamos que cada una de ellas pasen a ser columnas (variables) y que en 
# los valores aparezcan 0 y 1 cuando correspondan

# un ejemplo sería algo así:

# IDTUM  Quimioterapia  Radioterapia  Cirugia Otro
#   500              1             1        0    0
#   501              0             1        0    0
#   502              0             1        1    0

# almacenaremos la transformación en un dataframe llamado tabla

tabla <- rita_completo |> 
  filter(!is.na(ESTTON) & ESTTON != "Ignorado") |> 
  count(IDPTE, IDTUM, ESTTON) |> 
  mutate(n = if_else(n > 0, 1, 0)) |> 
  pivot_wider(names_from = ESTTON, values_from = n , values_fill = 0) |> 
  select(-IDPTE)

# veamos cada uno de los pasos
# 1.- filtramos los valores NA y la categoría "Ignorado" de ESSTON
# 2.- contabilizamos la frecuencia de ESTTON por cada combinación de IDPTE 
#     y IDTUM
# 3.- transformamos a 0 cuando el conteo es 0 y 1 cuando es 1 o más
# 4.- pivoteamnos la tabla de frecuencia a lo "ancho", tomando en cuenta
#     a ESTTON como names_from y n como values_from, rellenando con 0 cuando
#     el valor haya que completarlo por sus ausencia implicita
# 5.- quitamos la variable IDPTE

# la estructura de tabla es:

tabla

# antes de usar upset() sebemos convertir el tipo de tabla en dataframe solo
# esto es porque la función upset() de UpSetR requiere que los datos esten en
# un dataframe de R base (no funciona con dataframes del tidyverse)

tabla <- as.data.frame(tabla)

# finalmente aplicamos upset() para construir el gráfico

upset(tabla)

# podemos configurar algunos argumentos para mejorar la salida

upset(tabla, 
      nsets = 6, 
      order.by = "freq", 
      text.scale = 2)


# Guardar o exportar tablas de datos

# imaginemos que necesitemos exportar los datos de tabla
# lo vamos a realizar en dos formatos:

# Formato texto plano

write_csv2(x = tabla, 
           file = "practicas/tabla.csv")

# Formato Excel

library(openxlsx)

write.xlsx(x = tabla, 
           file = "practicas/tabla.xlsx")





