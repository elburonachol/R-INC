# Esta practica pertenece al temario del Encuentro 4 del curso

# La idea es ver algunas otras funciones o aplicaciones de lo
# mostrado durante el encuentro sincrónico

# Este código está comentado a modo de guía 

#-----------------------

# Activamos paquetes

library(tidyverse)


# leemos datos

guess_encoding("practicas/tsomf_prueba.csv")

# el encondign mayoritario del archivo es ISO-8859-1

tsomf <- read_csv2("practicas/tsomf_prueba.csv", 
                locale = locale(encoding = "ISO-8859-1"))

# exploramos estructura 

glimpse(tsomf)

# cadenas de caracteres



# fechas

# hay varias variables de tipo fecha dentro de la tabla de datos
# pero en la lectura fueron interpretadas como caracteres, porque
# el formato dd/mm/aaaa con la que están configuradas no son más
# que caracteres alfanuméricos
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

