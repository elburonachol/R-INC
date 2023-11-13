# Esta practica pertenece al temario del Encuentro 6 del curso

# La idea es ver algunas otras funciones o aplicaciones de lo
# mostrado durante el encuentro sincrónico

# Este código está comentado a modo de guía 

#-----------------------

# Activamos paquetes

library(tidyverse)
library(rstatix)
library(gtsummary)
library(flextable)

# leemos el archivo rita_prueba.csv

rita <- read_csv2("practicas/rita_prueba.csv")

# utilizaremos la función across() para convertir 
# tipo de datos en forma masiva algunos formatos de variables

# los nombres de las fechas comienzan con "FE"

rita |> 
  select(starts_with("FE")) 
  
# algunos valores donde no hay datos figuran con los caracteres
# "N/D" deberíamos cambiarlo por valores NA 

# esto se puede hacer directamente en la lectura de los datos
# utilizando el argumento na

rita <- read_csv2("practicas/rita_prueba.csv", 
                  na = c("", "NA", "N/D"))

# le estamos indicando que tanto los valores vacíos "" como los
# que dicen "NA" y "N/D" serán convertidos en valores NA

rita |> 
  select(starts_with("FE")) 

# ahora tenemos las 8 variables con formato caracteres con fechas

# para poder convertir todo en una sola operación podemos hacer:

rita <- rita |> 
  mutate(across(.cols = starts_with("FE"), .fns = dmy))

# verificamos

rita |> 
  select(starts_with("FE")) 

# convertimos 8 variables en date en un solo paso

# lo mismo se puede hacer con otras variables, como por
# ejemplo variables de caracter terminadas con N donde estan
# los nombres de ciertas variables categóricas

rita |> 
  select(ends_with("N") & where(is.character)) 

rita <- rita |> 
  mutate(across(.cols = ends_with("N") & where(is.character), 
                .fns = as_factor))

# corroboramos seleccionando por factor

rita |> 
  select(ends_with("N") & where(is.factor)) 

# estos permite en pocos pasos poder convertir grupos 
# numerosos de variables

# medidas resumen de una variable cuanti de rita

rita |> 
  get_summary_stats(DGED)

# frecuencia de una variable cuali de rita

rita |> 
  freq_table(OSPTE) |> 
  arrange(desc(prop))

# además de lo mostrado en el script del encuentro sobre 
# las virtudes de gtsummary mostraremos algunos ejemplos
# más complejos documentados

# tabla resumen de variable cuantitativa estratificada por
# variable categórica

rita  |> 
  select(DGED, PTEDU) |> 
  filter(PTEDU %in% c("Primario Incompleto", 
                      "Primario Completo",
                      "Secundario Incompleto", 
                      "Secundario Completo")) |> 
  mutate(PTEDU = fct_relevel(PTEDU, c("Primario Incompleto", 
                                      "Primario Completo", 
                                      "Secundario Incompleto",
                                      "Secundario Completo"))) |> 
  tbl_summary(
    by = PTEDU,
    type = all_continuous() ~ "continuous2", 
    label = list(DGED ~ "Edad al diagnóstico"),
    statistic = all_continuous() ~ c(
      "{mean} ({sd})",
      "{median} ({p25}, {p75})",
      "{min}, {max}"
    ), 
    missing = "no") |> 
  modify_header(label ~ "**Variable**") |> 
  italicize_levels()
  
# documentacion

# rita  |> 
# select(DGED, PTEDU) |> 
#  filter(PTEDU %in% c("Primario Incompleto", 
#                      "Primario Completo",
#                      "Secundario Incompleto", 
#                      "Secundario Completo")) |> 
#  mutate(PTEDU = fct_relevel(PTEDU, c("Primario Incompleto", 
#                                      "Primario Completo", 
#                                      "Secundario Incompleto",
#                                      "Secundario Completo")))

# en estas líneas seleccionamos las variables en juego y
# hacemos un recorte de algunas categorías de PTEDU
# como es una variable ordinal y el orden alfabetico natural
# no coincide con el orden real, se usa fct_relevel para cambiar 
# el orden de sus niveles


# tbl_summary(
#   by = PTEDU,
#   type = all_continuous() ~ "continuous2", 
#   label = list(DGED ~ "Edad al diagnóstico"),
#   statistic = all_continuous() ~ c(
#     "{mean} ({sd})",
#     "{median} ({p25}, {p75})",
#     "{min}, {max}"
#   ),
#   missing = "no"
# )


# usamos la función tbl_summary() con los argumentos
# by = para estratificar por PTEDU
# type = los resumenes se muestras en mas de una fila
# label = para cambiar la etiqueta de DGED
# statistic = pedimos la media con el desvio, la mediana y los
# quantiles 25 y 75 y el minimo y máximo
#  missing = "no", no muestra los valores NA

# modify_header(label ~ "**Variable**") |> 
#   italicize_levels()

# modificamos la cabecera con Variable en negrita
# hacemos italica las fuentes de las medidas
  
# otro ejemplo: 

rita  |> 
  filter(ECOG != "Desconocido") |> 
  select(PTESXN, ECOG) |> 
  tbl_summary(
    by = PTESXN,
    missing = "no",
    label = list(PTESXN ~ "Sexo", ECOG ~ "Status ECOG")
  ) |> 
  modify_header(label ~ "") 

# documentación

# rita  |> 
#   filter(ECOG != "Desconocido") |> 
#   select(PTESXN, ECOG) |> 
  
# seleccionamos y filtramos las variables que participan de
# la tabla

# tbl_summary(
#   by = PTESXN,
#   missing = "no",
#   label = list(PTESXN ~ "Sexo", ECOG ~ "Status ECOG")
# ) |> 
# modify_header(label ~ "") 

# usamos tbl_summary con

# by = variable de estratificación
# missing = "no" indica que los NA no serán tenidos en cuenta
# label = etiquetamos las variables
# modify_header, modificamos la cabecera quitando el texto

# tabla cruce de dos variables categóricas

# función tbl_cross()

rita  |> 
  select(PTESXN, PTEDU) |> 
  filter(PTEDU %in% c("Primario Incompleto", 
                      "Primario Completo",
                      "Secundario Incompleto", 
                      "Secundario Completo")) |> 
  mutate(PTEDU = fct_relevel(PTEDU, c("Primario Incompleto", 
                                      "Primario Completo", 
                                      "Secundario Incompleto",
                                      "Secundario Completo"))) |> 
  tbl_cross(
    row = PTEDU,
    col = PTESXN,
    label = list(PTEDU = "Nivel educativo",
                 PTESXN = "Sexo"),
    percent = "cell"
  ) |> 
  bold_labels()

# documentación

# rita  |> 
#   select(PTESXN, PTEDU) |> 
#   filter(PTEDU %in% c("Primario Incompleto", 
#                       "Primario Completo",
#                       "Secundario Incompleto", 
#                       "Secundario Completo")) |> 
#   mutate(PTEDU = fct_relevel(PTEDU, c("Primario Incompleto", 
#                                       "Primario Completo", 
#                                       "Secundario Incompleto",
#                                       "Secundario Completo"))) |> 

# mismo trabajo que en el primer ejemplo

#   tbl_cross(
#     row = PTEDU,
#     col = PTESXN,
#     label = list(PTEDU = "Nivel educativo",
#                  PTESXN = "Sexo"),
#     percent = "cell"
#   ) |> 
# bold_labels()

# usamos la función tbl_cross() para hacer cruces de variables
# categóricas, con los argumentos:

# row = la variable de la fila
# col = la variable de la columna
# label = las etiquetas de las variables
# percent = el formato del porcentaje (uno en cada celda)

# bold_labels() - hace que las variables aparezcan en negrita

# El paquete flextable ofrece un numeroso conjunto de funciones
# para trabajar con tablas que luego se pueden exportar 
# a formatos interesantes como Word o html

# tabla tipo dataframe

# por ejemplo un recorte de una pequeña tabla de rita
# creando una variable DEMORA entre fecha de inicio y
# fecha de consulta y filtrando las demoras de 5 a 10
# días (mostramos las 10 primeras)

rita |> 
  mutate(DEMORA = interval(FEINI, FECON)%/%ddays()) |> 
  select(IDPTE, IDTUM, FEINI, FECON, DEMORA) |> 
  filter(between(DEMORA, 5, 10)) |> 
  slice(1:10)

# el mismo código en lugar de salir por consola
# lo convertimos en objeto flextable y es mostrado
# en el Viewer (tipo html predeterminado)

rita |> 
  mutate(DEMORA = interval(FEINI, FECON)%/%ddays()) |> 
  select(IDPTE, IDTUM, FEINI, FECON, DEMORA) |> 
  filter(between(DEMORA, 5, 10)) |> 
  slice(1:10) |> 
  flextable()

# le queremos cambiar el aspecto al ancho de las columnas
# y que ajusten automáticamente 

rita |> 
  mutate(DEMORA = interval(FEINI, FECON)%/%ddays()) |> 
  select(IDPTE, IDTUM, FEINI, FECON, DEMORA) |> 
  filter(between(DEMORA, 5, 10)) |> 
  slice(1:10) |> 
  flextable() |> 
  autofit()

# le cambiamos el marcador de miles a IDPTE e IDTUM

rita |> 
  mutate(DEMORA = interval(FEINI, FECON)%/%ddays()) |> 
  select(IDPTE, IDTUM, FEINI, FECON, DEMORA) |> 
  filter(between(DEMORA, 5, 10)) |> 
  slice(1:10) |> 
  flextable() |> 
  autofit() |> 
  colformat_double(big.mark = "", digits = 0) 
  
# cambiamos la presentación de las fechas al formato
# dd/mm/aaaa

rita |> 
  mutate(DEMORA = interval(FEINI, FECON)%/%ddays()) |> 
  select(IDPTE, IDTUM, FEINI, FECON, DEMORA) |> 
  filter(between(DEMORA, 5, 10)) |> 
  slice(1:10) |> 
  flextable() |> 
  autofit() |> 
  colformat_double(big.mark = "", digits = 0) |> 
  colformat_date(fmt_date = "%d/%m/%Y")

# igual pero con otro formato de fecha

rita |> 
  mutate(DEMORA = interval(FEINI, FECON)%/%ddays()) |> 
  select(IDPTE, IDTUM, FEINI, FECON, DEMORA) |> 
  filter(between(DEMORA, 5, 10)) |> 
  slice(1:10) |> 
  flextable() |> 
  colformat_double(big.mark = "", digits = 0) |> 
  colformat_date(fmt_date = "%d %B %Y") |> 
  autofit() 

# guardamos la tabla

tabla_demora <- rita |> 
  mutate(DEMORA = interval(FEINI, FECON)%/%ddays()) |> 
  select(IDPTE, IDTUM, FEINI, FECON, DEMORA) |> 
  filter(between(DEMORA, 5, 10)) |> 
  slice(1:10) |> 
  flextable() |> 
  colformat_double(big.mark = "", digits = 0) |> 
  colformat_date(fmt_date = "%d %B %Y") |> 
  autofit() 

# objeto de clase flextable

class(tabla_demora)



# exportamos la tabla en html

save_as_html(tabla_demora, 
             path = "practicas/demora.html")


# exportamos la tabla en docx

save_as_docx(tabla_demora, 
             path = "practicas/demora.docx")

# cambiamos el formato de salida con el paquete officer

# officer nos permite manipular documentos de Word y PowerPoint

library(officer)

# en este caso creamos unas características de documento
# cambiando la disposición apaisada, un ancho y alto específico
# en pulgadas y margenes definidos

propiedades_seccion <- prop_section(
  page_size = page_size(
    orient = "landscape",
    width = 8.3, height = 11.7
  ),
  type = "continuous",
  page_margins = page_mar()
)

# exportamos en demora2.docx con estas especificaciones
# de página en Word

save_as_docx(tabla_demora, 
             path = "practicas/demora2.docx",
              pr_section = propiedades_seccion
)
