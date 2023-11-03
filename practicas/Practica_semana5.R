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

# formato Excel con otros argumentos más específicos

# Formato Excel

library(openxlsx)

# nombre de la hoja

write.xlsx(x = rita_sin_tratamiento, 
           file = "practicas/salida.xlsx", sheetName = "datos")

# existen otros elementos de control del Excel
# que se pueden visualizar en la documentación 

help("buildWorkbook")
