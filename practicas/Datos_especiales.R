## Gestión de datos especiales

# Activamos paquete

library(tidyverse)


# Leemos tabla de datos en Excel

library(readxl) # activamos paquete necesario

hpv <- read_excel("practicas/Informe_HPV_anonimizado.xlsx")

# Fechas

# detección de variables tipo fecha

# la función colnames() me devuelve el nombre de columna

hpv |> 
  select(where(is.Date)) |> 
  colnames()

# no hay ninguna tipo Date

hpv |> 
  select(where(is.POSIXct)) |> 
  colnames()

# hay 6 tipo Posix

hpv |> 
  select(where(is.POSIXct)) 

# de las 6 tipo fecha/hora sus horas
# las vemos en 00:00:00, dado que son solo fechas

# Existe alguna otra variable de fecha mal leída?

# Si observamos el diccionario de datos encontraremos que
# varias variables asociadas a pap deberíann ser de tipo fecha

# Por ejemplo, pap_previo:

hpv |> 
  pull(pap_previo) |> 
  class()

hpv |> 
  count(pap_previo, sort = T)

# como Excel no es un programa de bases de datos permite que 
# las celdas de una columna tengan diferentes tipos de datos
# en este caso, caracteres como NO junto a fechas
# la lectura en R va a coercionar al tipo posible (caracter)

# Una herramienta eficiente para transformar estos números 
# representativos de fechas es excel_numeric_to_date() del
# paquete janitor


library(janitor)

# primero convertimos en NA los valores "NO" y 
# luego aplicamos excel_numeric_to_date() 

hpv <- hpv |>
  mutate(pap_previo = na_if(pap_previo, "NO"),
         pap_previo = excel_numeric_to_date(as.numeric(pap_previo)))

hpv |> 
  pull(pap_previo) |> 
  class()

# Quitar hora a variables que no la necesitan

hpv |> 
  select(fechanacimiento)

# convertimos de POSIX a Date

hpv <- hpv |> 
  mutate(fechanacimiento = as_date(fechanacimiento)) 

hpv |> 
  select(fechanacimiento)

# Operaciones con fechas

# Calculo de edad al dia de hoy

hpv |> 
  mutate(edad_calculada = interval(start = fechanacimiento, end = today()),
         edad_calculada = edad_calculada %/% dyears()) |> 
  select(fechanacimiento, edad, edad_calculada)

# extracción del año / año epidemiológico

hpv |> 
  select(biopsia_ultima_fechamuestra)

# reparamos variable

hpv <- hpv |>
  mutate(biopsia_ultima_fechamuestra = na_if(biopsia_ultima_fechamuestra, "NO"),
         biopsia_ultima_fechamuestra = excel_numeric_to_date(as.numeric(biopsia_ultima_fechamuestra)))

# extraemos año 

library(skimr)

hpv |> 
  mutate(año_ultima_biopsia = year(biopsia_ultima_fechamuestra)) |> 
  skim(año_ultima_biopsia)

# extraemos semana y año epidemiológico

hpv |> 
  mutate(sepi_ultima_biopsia = epiweek(biopsia_ultima_fechamuestra),
         añoepi_ultima_biopsia = epiyear(biopsia_ultima_fechamuestra)) |> 
  select(biopsia_ultima_fechamuestra, sepi_ultima_biopsia, añoepi_ultima_biopsia) |> 
  arrange(biopsia_ultima_fechamuestra)

# Factores

# variables con categorías cerradas

hpv |> 
  pull(centrosalud) |> 
  class()

# conversión al tipo correcto

hpv <- hpv |> 
  mutate(centrosalud = factor(centrosalud))

# corroboramos conversión

hpv |> 
  pull(centrosalud) |> 
  class()

# vemos sus niveles

hpv |> 
  pull(centrosalud) |> 
  levels()

# tabla de frecuencia

hpv |> 
  count(centrosalud, sort = T)

# fusionamos menos frecuentes

hpv |>  
  mutate(centrosalud = fct_lump_n(centrosalud, 
                                  n = 10, 
                                  other_level = "OTRO")) |> 
  count(centrosalud)

# cambiamos niveles por frecuencia

hpv |> 
  pull(centrosalud) |> 
  fct_infreq() |> 
  levels()

# verificamos con tabla de frecuencia

hpv |> 
  count(centrosalud, sort = T)


# Valores NA con naniar

# Dado que en la tabla de datos hpv existen pocos valores NA,
# esta segunda parte del script la llevaremos a cabo sobre
# el archivo rita_prueba.csv

rita <- read_csv2("practicas/rita_prueba.csv")

library(naniar)

# algunas de las funciones de detección de valores NA más útiles son:

miss_var_summary(rita)

# rapidamente nos muestra en una tabla las variables ordenadas
# de mayor a menor frecuencia con el conteo absoluto de valores NA
# y el porcentaje respecto de la totalidad de observaciones.

gg_miss_upset(rita, 
              nsets = 10, 
              nintersects = 10)

# construye un gráfico tipo Upset donde vemos la intersección
# de variables y observaciones con valores NA
# se observan combinaciones de observaciones con valores NA en
# mas de una variable

# los argumentos nsets y nintersects permiten que configuremos 
# la inclusión de la cantidad de variables y combinaciones

# las funciones se integran con las conocidas del tidyverse

# creamos un vector de nombres de variables de rita con
# porcentajes de NA menores a 40 % 

variables <- miss_var_summary(rita) |> 
  filter(pct_miss < 40) |> 
  pull(variable)

variables 

# utilizamos el vector para seleccionar esas variables
# dentro del gráfico gg_miss_upset

rita |> 
  select(any_of(variables)) |> 
  gg_miss_upset(nsets = 10, 
              nintersects = 10)

# Caracteres 

# podemos trabajar con los valores del CIE-O de la variable
# TPGF 

# si queremos construir una variable con el código CIE-O 2 dígitos
# podemos utilizar funciones propias de stringr

# str_sub()

rita <- rita |> 
  mutate(IdGrpTopo = str_sub(TPGF, start = 1, end = 3)) 

rita |> 
  select(TPGF, IdGrpTopo)

# a partir de la nueva variable creamos otra con las categorías
# asociadas a esos códigos

# mostramos un recorte de esa labor utilizando str_detect() y
# expresiones regulares simples

rita |> 
  mutate(IdGrpTopo_txt = case_when(
    str_detect(IdGrpTopo, "C0[3-6]") ~ "Boca",
    str_detect(IdGrpTopo, "C[18|19|20]") ~ "Colon-recto",
    str_detect(IdGrpTopo, "C4[7-9]") ~ "Tu. malig Tej blando",
    str_detect(IdGrpTopo, "C4[0-1]") ~ "Hueso",
    str_detect(IdGrpTopo, "C7[0-2]") ~ "Encefalo_SNC",
    .default = "Otros"
  )) |> 
  distinct(IdGrpTopo, IdGrpTopo_txt) |> 
  count(IdGrpTopo_txt) 
  

