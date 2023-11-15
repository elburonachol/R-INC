# Gráficos - Encuentro 7

# activamos tidyverse

library(tidyverse)

# leemos datos

guess_encoding("practicas/tsomf_prueba.csv")

# el encondign mayoritario del archivo es ISO-8859-1

tsomf <- read_csv2("practicas/tsomf_prueba.csv", 
                   locale = locale(encoding = "ISO-8859-1"))

glimpse(tsomf)

# Gráficos de variables cualitativas

# Gráficos de barras con sexo

tsomf |> 
  ggplot(aes(x = sexo, fill = sexo)) + 
  geom_bar() + 
  scale_fill_manual(values = c("purple", "orange")) +
  theme(legend.position = "none")

# geom_bar() hace automáticamente un recuento de repeticiones de valores, 
# es decir que el eje y (count) se calcula en la misma operación del gráfico

# Si llegasemos al gráfico con una tabla con las frecuencias calculadas

tsomf |> 
  count(sexo) 

# debemos indicarle a la coordenada y de donde saca la frecuencia e
# incorporar dentro de la capa geométrica geom_bar() el arumento
# stat = "identity"

tsomf |> 
  count(sexo) |> 
  ggplot(aes(x = sexo, y = n, fill = sexo)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("purple", "orange")) +
  theme(legend.position = "none")

# Escalas de eje

# se pueden cambiar las escalas de eje con scale_x_* y scale_y_*
# el asterisco se reemplaza por la caracteristica de los datos asociados
# a ese eje

# por ejemplo, el eje y es un eje contínuo (count) por lo tanto:

tsomf |> 
  ggplot(aes(x = sexo, fill = sexo)) + 
  geom_bar() + 
  scale_fill_manual(values = c("purple", "orange")) +
  theme(legend.position = "none") + 
  scale_y_continuous(name = "Frecuencia", 
                     limits = c(0,80), 
                     breaks = seq(0, 80, by = 10))

# se puede declarar dentro de la escala el nombre del eje, los límites y 
# las separaciones con sus marcaciones de escala

# para el argumento breaks de escalas contínuas es muy útil la función seq()


# una capa interesante para agregar a este tipo de gráficos es una de texto 
# o de etiquetas

tsomf |> 
  count(sexo) |> 
  ggplot(aes(x = sexo, y = n, fill = sexo)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("purple", "orange")) +
  theme(legend.position = "none") +
  geom_text(aes(label = n), nudge_y = 3) 

# para sumar etiquetas conviene que el gráfico provenga de una tabla de conteo
# a la capa geométrica text se le agrega un aes() para mapear la propiedad
# label igualando a n
# también se puede definir el desplazamiento del texto para que no se superponga
# con las barras (nugde_y desplazamiento vertical, nudge_x horizontal)

# igual pero con geom_label()

tsomf |> 
  count(sexo) |> 
  ggplot(aes(x = sexo, y = n)) + 
  geom_bar(aes(fill = sexo), stat = "identity") + 
  scale_fill_manual(values = c("purple", "orange")) +
  theme(legend.position = "none") +
  geom_label(aes(label = n), nudge_y = 3) 

# rotar coordenadas

tsomf |> 
  count(sexo) |> 
  ggplot(aes(x = sexo, y = n, fill = sexo)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("purple", "orange")) +
  theme(legend.position = "none") +
  geom_text(aes(label = n), nudge_y = 3) +
  coord_flip()

# coord_flip() cambia la disposición del gráfico

# barras combinadas

# para incluir otra variable categórica en las barras debemos utilizar
# el mapeo de otra caracteristica como fill

tsomf |> 
  count(sexo, resultado) 

tsomf |> 
  count(sexo, resultado) |> 
  filter(resultado != "Inadecuado") |> 
  ggplot(aes(x = sexo, y = n, fill = resultado)) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values = c("springgreen4", "salmon2"))

# por defecto la posición es "stack", es decir una categoría encima de la otra
# con valores absolutos de frecuencia

# lo podemos cambiar con "dodge"

tsomf |> 
  count(sexo, resultado) |> 
  filter(resultado != "Inadecuado") |> 
  ggplot(aes(x = sexo, y = n, fill = resultado)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("springgreen4", "salmon2"))

# o "fill" (similar a stack con valores relativos)

tsomf |> 
  count(sexo, resultado) |> 
  filter(resultado != "Inadecuado") |> 
  ggplot(aes(x = sexo, y = n, fill = resultado)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values = c("springgreen4", "salmon2"))

# si queremos hacer como el gráfico con etiquetas debemos adaptar la 
# capa geométrica a la disposición del gráfico

tsomf |> 
  count(sexo, resultado) |> 
  filter(resultado != "Inadecuado") |> 
  ggplot(aes(x = sexo, y = n, fill = resultado)) + 
  geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_manual(values = c("springgreen4", "salmon2")) +
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(limits = c(0, 55))

# definimos posición en dodge y ajustamos el desplazamiento vertical
# además aumentamos los límites de la escala para que se vea la etiqueta

# gráficos de torta

# parte de un gráfico apilado al 100% de una sola barra

tsomf |> 
  count(sexo) |>
  ggplot(aes(x = "", y = n, fill = sexo)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values = c("violetred", "dodgerblue2")) +
  geom_text(aes(label = round(n/sum(n)*100, 1)), 
            position = position_stack(vjust = 0.5))

# la clave está en cambiar el sistema de coordenadas a polar

tsomf |> 
  count(sexo) |>
  ggplot(aes(x = "", y = n, fill = sexo)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(values = c("violetred", "dodgerblue2")) +
  geom_text(aes(label = paste0(round(n/sum(n)*100, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +
  coord_polar("y", start=0) +
  theme_void() 
    

# Gráficos de variables cuantitativas

# vamos a calcular la edad a partir de la fecha de nacimiento para poder
# trabajar con ella

tsomf <- tsomf |> 
  mutate(edad = interval(dmy(fecha_nacimiento), dmy(fecha_test))%/%dyears())

# histogramas

# solo necesitamos declarar en x la variable cuantitativa para construir un
# histograma

tsomf |> 
  ggplot(aes(x = edad)) +
  geom_histogram(binwidth = 5, 
                 fill = "firebrick",
                 color = "white") 
   
# definimos ancho de 5 años y colores de relleno y contorno

# binwidth llama internamnete a cut_width() del propio ggplot2

tsomf |> 
  mutate(intervalo5 = cut_width(edad, width = 5, center = 0))

# podemos graficar lo mismo con geom_bar()

tsomf |> 
  mutate(intervalo_edad = cut_width(edad, width = 5, center = 0)) |> 
  count(intervalo_edad) |> 
  ggplot(aes(x = intervalo_edad, y = n)) +
  geom_bar(stat = "identity", 
           fill = "firebrick",
           color = "white")

# como la variable intervalo de edad es continua debemos hacer que
# las barras se toquen y además acomodar mejor las etiquetas de x

tsomf |> 
  mutate(intervalo_edad = cut_width(edad, width = 5, center = 0)) |> 
  count(intervalo_edad) |> 
  ggplot(aes(x = intervalo_edad, y = n)) +
  geom_bar(stat = "identity", 
           fill = "firebrick",
           color = "white", 
           width = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# en cambio si queremos que el ancho de la barra del histograma
# coincida con grupos etarios en enteros (de 5 en 5 por ejemplo)

tsomf |> 
  mutate(intervalo5 = cut_interval(edad, length =  5)) |> 
  count(intervalo5)

tsomf |> 
  mutate(intervalo5 = cut_interval(edad, length =  5)) |> 
  count(intervalo5) |> 
  ggplot(aes(x = intervalo5, y = n)) +
  geom_bar(stat = "identity", 
           fill = "firebrick",
           color = "white", 
           width = 1) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

# distribución de una variable cuantitativa segun una categórica

# veamos la categórica

tsomf |> 
  count(centrosalud_informe)

# achiquemos las categorías reduciendo a partir de n = 2

tsomf <- tsomf |> 
  mutate(centro_salud = fct_lump_n(centrosalud_informe, n = 2, other_level = "Otros")) 

# verifiquemos

tsomf |> 
  count(centro_salud)

# ahora tenemos 3 categorías

# la capa de puntos se superponen en la misma línea vertical

tsomf |> 
  ggplot(aes(x = centro_salud, y = edad)) +
  geom_point() 


# mejor es geom_jitter() que distribuye aleatoriamente a lo "ancho"

tsomf |> 
  ggplot(aes(x = centro_salud, y = edad)) +
  geom_jitter()

# se puede mostrar resúmenes estadísticos

tsomf |> 
  ggplot(aes(x = centro_salud, y = edad)) +
  geom_boxplot()

# o violines

tsomf |> 
  ggplot(aes(x = centro_salud, y = edad)) +
  geom_violin()

# o las dos capas juntas (boxplot + jitter)

tsomf |> 
  ggplot(aes(x = centro_salud, y = edad)) +
  geom_boxplot() +
  geom_jitter()

# algo más completo

tsomf |> 
  ggplot(aes(x = centro_salud, 
             y = edad,  
             fill = centro_salud)) +
  geom_boxplot(outlier.color = "red", alpha = 0.5) +
  geom_jitter(width = 0.2, shape = 1, size = 3) +
  xlab(label = "") +
  scale_fill_brewer(name = "Centro de Salud", 
                    palette = "Set1") +
  theme(legend.position = "bottom")

# Pirámides poblacionales

# preparación de los datos

# creamos intervalos cada 5 años
# contamos sexo e intervalos
# hacemos que los valores de hombres sea negativo

tabla_pob <- tsomf |> 
  mutate(grupo_edad = cut_interval(edad, length =  5)) |> 
  count(sexo, grupo_edad) |> 
  mutate(n = if_else(sexo == "F", n, n*-1))

# creamos gráfico de columnas invertido

tabla_pob |> 
  ggplot(aes(x = grupo_edad,
           y = n,
           fill = sexo)) +
  coord_flip() +
  geom_col() +
  scale_y_continuous(breaks  = seq(-20, 20, by = 5),
                     labels = abs(seq(-20, 20, by = 5))) +
  scale_fill_brewer(palette = "Accent",
                    name = "") +
  theme_minimal() +
  theme(legend.position = "bottom")

