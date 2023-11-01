# Esta practica pertenece al temario del Encuentro 5 del curso

# La idea es ver algunas otras funciones o aplicaciones de lo
# mostrado durante el encuentro sincrónico

# Este código está comentado a modo de guía 

#-----------------------

# Activamos paquetes

library(tidyverse)


# Creamos dos tablas pequeñas para la práctica

casos <- tribble(
  ~DNI,   ~edad, ~sexo, 
  23654738, 46,     "Mujer",
  12432098, 60,     "Varon",
  20987302, 50,     "Varon",
  17235092, 53,     "Mujer",
  5425810,  71,     "Mujer"
)

muertes <- tribble(
  ~DNI,   ~fecha,  
  23654738, "12/12/2018",    
  12432098, "03/10/2018",    
  20987400, "24/05/2018",    
  17235092, "08/02/2018",    
  5425990,  "16/03/2018" 
)

# Mutating joins (Uniones de transformación)

# En todos los casos asume a la variable DNI (única que se encuentra en las
# dos tablas) como clave

inner_join(casos, muertes) # unión interior

# uniones exteriores

left_join(casos, muertes) # agrega NA en variables de muertes

right_join(casos, muertes) # agrega NA en variables de casos

full_join(casos, muertes) # agrega NA en todas las variables


# Filtering joins (Uniones de filtro)

semi_join(casos, muertes) # Mantiene las observaciones donde DNI coincide

anti_join(casos, muertes) # Descarta las observaciones donde DNI coincide


# Que sucede cuando los nombres de las claves son distintas?

# modificamos la tabla muertes cambiando el nombre de la variable clave

muertes <- tribble(
  ~Documento,       ~fecha,  
  23654738, "12/12/2018",    
  12432098, "03/10/2018",    
  20987400, "24/05/2018",    
  17235092, "08/02/2018",    
  5425990,  "16/03/2018" 
)

# si intentamos la unión con la forma anterior, que nos devuelve?
inner_join(casos, muertes)

# Informa de un error porque las dos tablas no tienen al menos una variable 
# común

# Por lo que debemos indicarle en el argumento by el nombre de las variables 
# claves de cada tabla

inner_join(casos,muertes, by = c("DNI" = "Documento"))

# la forma es by = c("nombre clave tabla1" = "nombre clave tabla2")


# funciones bind_

# supongamos que tenemos tablas con la misma estructura que necesitamos
# unir por sus observaciones

casos_2010 <- tribble(
  ~Mes,       ~Casos,  
  5,              254,    
  6,              325,    
  9,              145,    
  10,              87,    
  12,              41
)

casos_2011 <- tribble(
  ~Mes,       ~Casos,  
  2,              34,    
  7,             321,    
  8,             314,    
  11,            102,    
  12,             64 
)

# en estos casos se pude utilizar bind_rows() pero si unimos
# vamos a tener el problema de meses no queda relacionado
# con el año al que pertenecen y por lo tanto, no sabremos
# a que momento pertenecen esas cantidades de casos.

# vamos a tener que hacer un par de operaciones anteriores

casos_2010 <- casos_2010 |> 
  mutate(año = 2010)

casos_2011 <- casos_2011 |> 
  mutate(año = 2011)

# ahora si podemos unir

casos_10_11 <- casos_2010 |> 
  bind_rows(casos_2011)

casos_10_11

# podemos tener todos los datos juntos pero con la identificación 
# del año al que pertenece cada uno


# datos ordenados (pivoteos)

# trabajamos con tablas de datos del paquete datos (deberá instalarlo)

library(datos)

tabla4a

# la tabla tiene como nombres de columnas las categorías de una
# variable "año"

# pivoteamos para corregir con pivot_longer()

tabla4a |>  
  pivot_longer(cols = 2:3, 
               names_to = "año", 
               values_to = "casos")

## Cambiamos el formato usando pivot_wider()

tabla2

# extraemos los nombres de las variables de la columna tipo
# y los valores de cuenta

tabla2 |> 
  pivot_wider(names_from = tipo, 
              values_from = cuenta)

# otras funciones útiles de tidyr

# existen otras funciones que en la gestión de tablas de datos
# pueden ser muy útiles

# separate()

# sirve para separar una columna en más de una. Necesita que los 
# valores de la columna original tengan un caracter separador en 
# todas las observaciones

# un ejemplo sencillo, que también viene incluído en el paquete datos
# es oms (información de OMS sobre TB)

# acomodemos la tabla para que tenga un formato vertical

oms1 <- oms |> 
  select(1:4, starts_with("nuevos_")) |> 
  filter(anio > 2000) |> 
  pivot_longer(
    cols = 5:46, 
    names_to = "clave", 
    values_to = "casos"
  )

# los valores de la columna clave tienen una estructura con
# separadores guión bajo, como vemos a continuación:

oms1

# en este caso la función es necesaria

oms1 |>
  separate(col = clave, 
           into = c("nuevos", "tipo", "sexo_edad"), 
           sep = "_")

# también provee una función llamada unite() que hace lo inverso

# Gestión de duplicados

# vimos en el script del encuentro la detección de duplicados
# diversos y la estrategia de solución utilizando distinct y
# slice_

# en esta oportunidad veremos como marcar los duplicados
# en la tabla de datos

# detección y marcado

# leemos una tabla con duplicados completos, por ejemplo
rita_sin_tratamiento <- read_csv2("practicas/rita_sin_tratamiento.csv")

# activamos paquete
library(janitor)

# detectamos duplicados completos y extraemos clave unívoca
# IDTUM en vector duplicado

duplicado <- get_dupes(rita_sin_tratamiento) |> 
  distinct(IDTUM) |> 
  pull()

# utilizamos vector duplicado en mutate para crear nueva
# variable marca y etiquetas duplicados y observaciones 
# únicas

rita_sin_tratamiento <- rita_sin_tratamiento |> 
                          mutate(marca = if_else(IDTUM %in% duplicado, 
                                                 "Duplicado", 
                                                 "Unico"))

# para verificar hacemos un count() a la variable marca

rita_sin_tratamiento |> 
  count(marca)





# Exportación de resultados










guess_encoding("practicas/tsomf_prueba.csv")

# el encondign mayoritario del archivo es ISO-8859-1

tsomf <- read_csv2("practicas/tsomf_prueba.csv", 
                locale = locale(encoding = "ISO-8859-1"))

# exploramos estructura 

glimpse(tsomf)

# crearemos otros datos particulares de práctica

datos <- tribble(
  ~SEXO, ~EDUCACION, ~GRUPO_EDAD, 
  "Varon", "Primario Incompleto", "0-9",
  "MUJER", "Secundario Completo", "10-19",
  "Mujer", "Secundario Completo", "40-49",
  "Varon", "Secundario Incompleto", "10-19",
  "Hombre", "Universitario Completo", "20-29"
)

# cadenas de caracteres

# A las funciones de stringr vistas en el encuentro le
# sumaremos otras para gestionar cadenas de caracteres 
# en el marco de la depuración de datos

# Observemos la variable SEXO de la tabla datos

datos |> 
  select(SEXO)

# Evidentemente hay categorías que conceptualmente son las
# mismas pero están escritas de diferente forma

# Si quisieramos hacer una tabla de frecuencia, pasaría esto:

datos |> 
  count(SEXO)

# tenemos 2 mujeres escritas con mayúsculas y minúsculas y
# 2 varones escritos como Varon y Hombre

# apliquemos un par de funciones de stringr para corregir esto

datos |> 
  mutate(SEXO = str_to_upper(SEXO),
         SEXO = str_replace(SEXO, "HOMBRE", "VARON")) |> 
  count(SEXO)

# en primer lugar la estructura de modificar variables se
# incluye dentro de un mutate
# luego usamos str_to_upper para pasar a todos los valores a
# mayúsculas unificando lo escrito
# en tercer lugar usamos str_replace para reemplazar
# el valor incorrecto 
# finalmente armamos la tabla de frecuencia para verificar

# habitualmente estas tareas de depuración se almacenan en
# una nueva tabla de datos depurados o bien en el mismo dataframe

# otra función útil cuando queremos editar variables con vistas
# a presentaciones en tablas o gráficos es str_glue()

datos |> 
  select(GRUPO_EDAD)

# si queremos tener etiquetas agregando la palabra años a 
# estos grupos etarios se puede hacer:

datos |> 
  mutate(GRUPO_EDAD = str_glue("{GRUPO_EDAD} años")) |> 
  select(GRUPO_EDAD)


# variables de tiempo - fechas

# hay varias variables de tipo fecha dentro de la tabla de datos
# leída (tsomf.csv) pero en la lectura fueron interpretadas como
# caracteres, porque el formato dd/mm/aaaa con la que están 
# configuradas no son más que caracteres alfanuméricos
# el formato interno que R reconoce como Date es aaaa-mm-dd

# vamos a transformar tres de ellas en el formato adecuado y 
# volver a guardarlas en tsomf

tsomf <- tsomf |> 
        mutate(fecha_nacimiento = dmy(fecha_nacimiento),
               fecha_test = dmy(fecha_test),
               fecha_entrega = dmy(fecha_entrega))

# verficamos el formato y la forma del contenido
tsomf |> 
  select(where(is.Date))

# se convirtieron correctamente (date - formato aaaa-mm-dd)

# funciones generales

today() # devuelve el día de la fecha actual 

now() # devuelve el día y la hora del momento actual

# el -03 que acompaña a la hora es el huso horario de nuestra
# ubicación (Argentina)

# operaciones de extracción

# podemos extraer partes de cualquier fecha, por ejemplo de
# fecha_test

tsomf |> 
  mutate(dia = day(fecha_test),
         dia_nombre = wday(fecha_test, label = T, abbr = F),
         semana = week(fecha_test),
         mes = month(fecha_test),
         mes_nombre = month(fecha_test, abbr = F, label = T),
         año = year(fecha_test)
         
         ) |> 
  select(fecha_test, dia, dia_nombre, semana, mes, mes_nombre, año)

# en materia de fechas relacionadas a vigilancia epidemiológica
# podemos extraer:

tsomf |> 
  mutate(semana_epidemiologica = epiweek(fecha_test),
         año_epidemiologico = epiyear(fecha_test)) |> 
  select(fecha_test, semana_epidemiologica, año_epidemiologico)

# el año epidemiologico se diferencia del año común cuando estamos
# cerca de fin de año y comienzo de uno nuevo, dado que si la semana
# final de un año pasa a tener fechas del comienzo del nuevo, esas
# fechas van a ser años nuevos comunes y años anteriores con epiyear()

# creación de lapsos de tiempo

# intervalos (se presentan como intervalos de fechas)

tsomf |> 
  mutate(intervalo = interval(start = fecha_nacimiento,
                              end = fecha_test)) |> 
  select(fecha_nacimiento, fecha_test, intervalo)

# duraciones (su unidad es el segundo)

tsomf |> 
  mutate(duracion = as.duration(interval(start = fecha_nacimiento,
                              end = fecha_test))) |> 
  select(fecha_nacimiento, fecha_test, duracion)

# períodos (sys unidades tienen diferentes unidades)

tsomf |> 
  mutate(periodo = as.period(interval(start = fecha_nacimiento,
                                         end = fecha_test))) |> 
  select(fecha_nacimiento, fecha_test, periodo)

# operaciones entre fechas

# calculo de la edad al momento del test

tsomf |> 
  mutate(edad_test = interval(start = fecha_nacimiento,
                              end = fecha_test) %/% dyears()) |> 
  select(fecha_nacimiento, fecha_test, edad_test)

# se divide el intervalo de fechas por la duración de un año
# función dyears(). El operador %/% hace división entera

# calculo de dias entre test y entrega

tsomf |> 
  mutate(demora = interval(start = fecha_entrega,
                              end = fecha_test) %/% ddays()) |> 
  select(fecha_entrega, fecha_test, demora)

# la función ddays() es la duración de un día

## Cambiamos unidades desde segundos hasta decadas

tsomf |> 
  mutate(edad_test = interval(start = fecha_nacimiento,
                              end = fecha_test),
         segundos = edad_test / dseconds(),
         minutos = edad_test / dminutes(),
         horas = edad_test / dhours(),
         dias =  edad_test / ddays(),
         semanas = edad_test / dweeks(),
         años = edad_test %/% dyears(),
         lustros = edad_test / dyears(5),
         decadas = edad_test / years(10)) |> 
  select(fecha_nacimiento, fecha_test, edad_test, segundos, minutos, horas, dias, semanas, años, lustros, decadas) |> 
  View()

# creamos intervalo de fechas y luego utilizamos difrentes
# funciones de duración para obtener el mismo tiempo en 
# unidades distintas

# factores

# volvemos con la tabla datos que usamos en la gestión de caracteres

datos

# las tres variables pueden ser vistas como variables con 
# categorías cerradas por lo tanto se pueden o, dependiendo
# del uso futuro, deben convertir en factor.

# veamos como ejemplo la variable EDUCACION

# si vamos a crear una tabla de frecuencia quedaría así:

datos |> 
  count(EDUCACION)

# resulta que el ordenamiento natural del R lleva a tener
# la categoría Secundario Completo antes que el Secundario 
# Incompleto, no respetando la ordinalidad de la variable

# esta situación, como otras, nos obligan a convertirla en
# factor, única forma en que podemos modificar el orden

# si convertimos directamente el orden será automático y
# estará mal al igual que cuando era de tipo caracter

datos |> 
  mutate(EDUCACION = factor(EDUCACION)) |> 
  count(EDUCACION)

# notese que debajo del nombre de la variable dice fct 
# que significa factor

# para indicarle los niveles correctos podemos declararlos
# en la misma conversión

datos |> 
  mutate(EDUCACION = factor(EDUCACION, 
                            labels = c("Primario Incompleto",
                                       "Secundario Incompleto",
                                       "Secundario Completo",
                                       "Universitario Completo"))) |> 
  count(EDUCACION)

# en este ejemplo no estamos declarando todos los posibles niveles
# dado que la tabla datos tiene un recorte de ellos

# también podemos hacerlo en dos pasos con la función fct_relevel 

datos |> 
  mutate(EDUCACION = factor(EDUCACION),
         EDUCACION = fct_relevel(EDUCACION,
                                 "Secundario Incompleto",
                                 after = 1)) |> 
  count(EDUCACION)

# primero transformamos en factor y luego
# con fct_relevel cambiamos los niveles diciendo que
# "Secundario Incompleto" va después de la primera posición

# otra función utilizada cuando necesitamos invertir el orden 
# de una variable de este tipo para, por ejemplo, la escala 
# de un gráfico es fct_rev()

datos |> 
  mutate(EDUCACION = factor(EDUCACION),
         EDUCACION = fct_relevel(EDUCACION,
                                 "Secundario Incompleto",
                                 after = 1),
         EDUCACION = fct_rev(EDUCACION)) |> 
  count(EDUCACION)
