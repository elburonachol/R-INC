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
  geom_histogram(binwidth = 5, fill = "firebrick", color = "white") 
   
# definimos ancho de 5 años y colores de relleno y contorno

# Pirámides poblacionales



