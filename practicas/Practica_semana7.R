# Esta practica pertenece al temario del Encuentro 7 del curso

# La idea es ver algunas otras funciones o aplicaciones de lo
# mostrado durante el encuentro sincrónico

# los gráficos siempre se dibujaran en el panel Plots de RStudio
# que tiene un boton de Zoom que al presionar abrirá una 
# ventana emergente que podemos maximizar al tamaño de la 
# pantalla (realicen esto para ver mejor las producciones gráficas)

# Este código está comentado a modo de guía 

#-----------------------

# Activamos paquetes

library(tidyverse)

# leemos el archivo rita_prueba.csv

rita <- read_csv2("practicas/rita_prueba.csv")


# gráfico de líneas para tendencias

# extraemos año de la fecha de registro

rita <- rita |> 
  mutate(año = year(dmy(FEREG)))

# agrupando por año contabilizamos segun id único

rita |> 
  group_by(año) |> 
  summarise(Pacientes = n_distinct(IDPTE),
            Tumores = n_distinct(IDTUM),
            Tratamientos = n_distinct(IDTTO))

# debemos "ordenar" los datos 
# para esto, pivoteamos de ancho a largo

rita |> 
  group_by(año) |> 
  summarise(Pacientes = n_distinct(IDPTE),
            Tumores = n_distinct(IDTUM),
            Tratamientos = n_distinct(IDTTO)) |> 
  pivot_longer(cols = 2:4, names_to = "tipo", values_to = "cantidad")

# ahora si podemos ingresar los datos al gráfico

tendencia <- rita |> 
  group_by(año) |> 
  summarise(Pacientes = n_distinct(IDPTE),
            Tumores = n_distinct(IDTUM),
            Tratamientos = n_distinct(IDTTO)) |> 
  pivot_longer(cols = 2:4, names_to = "tipo", values_to = "cantidad") |> 
  ggplot(aes(x = año, y = cantidad, color = tipo)) +
  geom_point(aes(shape = tipo), size = 3) +
  geom_line(linewidth = 0.7) 

# donde año estará en el eje x, cantidad en el eje y
# color será diferente según las categorías de tipo
# la forma del punto cambiará según tipo y será de tamaño 3
# las líneas tendran un grosor de 0.7

# finalmente almacenamos el gráfico en el objeto tendencia,
# que podemos visualizar

tendencia

# luego podemos ir agregando caracteristicas y capas al objeto
# mediante el operador +

# por ejemplo, agregamos escalas de ejes x e y

tendencia +
  scale_x_continuous(name = "Año", 
                     breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(name = "Frecuencia", 
                     limits = c(0, 150), 
                     breaks = seq(0, 150, by = 10))

# los argumentos habituales son:
# name = nombre del eje
# limits = limites de la escala
# breaks = cortes a mostrar en la escala

# ahora le podemos incorporar etiquetas (de titulo, subtitulo y pie)

tendencia <- tendencia +
  scale_x_continuous(name = "Año", 
                     breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(name = "Frecuencia", 
                     limits = c(0, 150), 
                     breaks = seq(0, 150, by = 10)) +
  labs(title = "Número de registros en RITA por año", 
       subtitle = "Argentina - 2012-2023" , 
       caption = "Fuente: SIVER-Ca en base a datos del RITA. INC")

# la función labs posibilita agregar etiquetas
# algunos de sus argumentos son
# title = titulo del gráfico
# subtitle = subtitulo del gráfico
# caption = pie de gráfico

tendencia

# además podemos cambiar el tema, ubicar la leyenda debajo y 
# sin titulo, y cambiar colores

tendencia <- tendencia +
  scale_color_manual(values = c("royalblue3", "peru", "forestgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

tendencia

# agregamos escalas de colores manuales
# en este ejemplo estamos utilizando colores de colors()

colors()

# hay 657 colores posibles almacenados dentro del lenguaje
# bajo estos nombres

# también podriamos usar colores en números hexadecimales 
# que se pueden sacar de sitios como https://www.colorhexa.com/

tendencia +
  scale_color_manual(values = c("#2e76ac", "#9400ff", "#85ae38")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

# por otra parte seleccionamos un tema minimo ()
# existen varios temas incorporados en ggplot2 y otros
# provenientes en varios paquetes 

# estos temas se pueden modificar, haciendo que la leyenda
# se ubique debajo del gráfico y que no aparezca el titulo
# como en este ejemplo

####

# esto generalmente se construye todo junto

rita |> 
  group_by(año) |> 
  summarise(Pacientes = n_distinct(IDPTE),
            Tumores = n_distinct(IDTUM),
            Tratamientos = n_distinct(IDTTO)) |> 
  pivot_longer(cols = 2:4, names_to = "tipo", values_to = "cantidad") |> 
  ggplot(aes(x = año, y = cantidad, color = tipo)) +
  geom_point(aes(shape = tipo), size = 3) +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(name = "Año", breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(name = "Frecuencia", limits = c(0,150), 
                     breaks = seq(0,150, by = 10)) +
  labs(title = "Número de registros en RITA por año", 
       subtitle = "Argentina - 2012-2023" , 
       caption = "Fuente: SIVER-Ca en base a datos del RITA. INC") +
  scale_color_manual(values = c("royalblue3", "peru", "forestgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())


# Personalizar temas gráficos

# podemos especificar ciertas caracteristicas generales de theme
# como tipo de fuentes, tamaños, etc

# la librería systemfonts tiene acceso a las fuentes nativas del S.O

library(systemfonts)

# mostramos las fuentes instaladas
system_fonts() |> View()

# una fuente habitualmente instalada es "Times New Roman"

system_fonts() |> 
  filter(family == "Times New Roman")

# guardamos las características del tema que usamos (como backup)

backup_tema <- theme_set(theme_minimal())

# vamos a visualizar todas las características del tema (son muchas)

backup_tema

# en texto la familia es ""

# seteamos el theme_minimal() con la familia "Times New Roman" 

theme_set(theme_minimal(base_family = "Times New Roman"))

# con theme_update() podemos actualizar otros elementos
# del tema seteado

theme_update( 
  axis.text.x = element_text(color = "black", face = "bold", size = 12, 
                             margin = margin(t = 6)),
  axis.text.y = element_text(color = "black", size = 12, hjust = 1, 
                             margin = margin(r = 6), family = "Times New Roman"), 
  plot.title = element_text(color = "black", face = "bold", size = 24, family = "Times New Roman"),
  plot.subtitle = element_text(color = "black", face = "bold", size = 20, family = "Times New Roman"),
  axis.line.x = element_line(color = "black", linewidth = 1),
  panel.grid.major.y = element_line(color = "grey90", linewidth = 0.6),
  plot.background = element_rect(fill = "white", color = "white"),
  legend.position = "bottom",
  legend.title = element_blank(),
  legend.text = element_text(color = "black", size = 12),
  plot.margin = margin(rep(20, 4))
)

# aquí estamos modificando la configuración del título, subtítulo,
# línea del eje x, grilla del eje y, fondo del gráfico, posición,
# titulo y texto de la leyenda y margenes del gráfico

# aplicamos este tema en el gráfico

rita |> 
  group_by(año) |> 
  summarise(Pacientes = n_distinct(IDPTE),
            Tumores = n_distinct(IDTUM),
            Tratamientos = n_distinct(IDTTO)) |> 
  pivot_longer(cols = 2:4, names_to = "tipo", values_to = "cantidad") |> 
  ggplot(aes(x = año, y = cantidad, color = tipo)) +
  geom_point(aes(shape = tipo), size = 3) +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(name = "Año", breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(name = "Frecuencia", limits = c(0,150), 
                     breaks = seq(0,150, by = 10)) +
  labs(title = "Número de registros en RITA por año", 
       subtitle = "Argentina - 2012-2023" , 
       caption = "Fuente: SIVER-Ca en base a datos del RITA. INC") +
  scale_color_manual(values = c("royalblue3", "peru", "forestgreen")) 

# los cambios en el tema desaparecen cuando iniciamos una nueva 
# sesión de R o bien cuando recuperamos el tema minimal original 
# almacenado, como en la siguiente línea:

theme_set(backup_tema)

# verificamos el tema original seteado

rita |> 
  group_by(año) |> 
  summarise(Pacientes = n_distinct(IDPTE),
            Tumores = n_distinct(IDTUM),
            Tratamientos = n_distinct(IDTTO)) |> 
  pivot_longer(cols = 2:4, names_to = "tipo", values_to = "cantidad") |> 
  ggplot(aes(x = año, y = cantidad, color = tipo)) +
  geom_point(aes(shape = tipo), size = 3) +
  geom_line(linewidth = 0.7) +
  scale_x_continuous(name = "Año", breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(name = "Frecuencia", limits = c(0,150), 
                     breaks = seq(0,150, by = 10)) +
  labs(title = "Número de registros en RITA por año", 
       subtitle = "Argentina - 2012-2023" , 
       caption = "Fuente: SIVER-Ca en base a datos del RITA. INC") +
  scale_color_manual(values = c("royalblue3", "peru", "forestgreen")) 

# Graficos de sectores

# usamos una categórica con pocas categorías

rita |> 
  rstatix::freq_table(PTESXN)

# los valores de la tabla de frecuencia de rstatix pueden servir
# porque incluye facilmente las proporciones

rita  |> 
  rstatix::freq_table(PTESXN) |>
  ggplot(aes(x = "", y = n, fill = PTESXN)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Sexo",
                    values = c("violetred", "dodgerblue2")) +
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +
  coord_polar("y", start=0) +
  theme_void() 

# en el gráfico usamos la variable prop calculada por freq_table()
# para visualizar las etiquetas de geom_label(), redondeadas y con
# 1 decimal. Esto lo encerramos en la función paste0() que une texto
# sin espacio entre sus componentes para agregar el signo %

# cambiamos la disposición de inicio de los sectores de la torta

# el argumento start dentro de la función coord_polar() permite
# decidir donde comienza a dibujarse los sectores
# también direction determina la dirección (1 = dirección horaria,
# -1 dirección antihoraria)
# los valores de start se miden en radianes 
# 1 grado es igual 0.0174533 radianes

# ejemplo comenzando en 90 grados sentido horario

rita  |> 
  rstatix::freq_table(PTESXN) |>
  ggplot(aes(x = "", y = n, fill = PTESXN)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Sexo",
                    values = c("violetred", "dodgerblue2")) +
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +
  coord_polar("y", start=90*0.0174533, direction = 1) +
  theme_void() 

# igual sentido antihorario - cambiamos text a label

rita  |> 
  rstatix::freq_table(PTESXN) |>
  ggplot(aes(x = "", y = n, fill = PTESXN)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Sexo", 
                    values = c("violetred", "dodgerblue2")) +
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +
  coord_polar("y", start=90*0.0174533, direction = -1) +
  theme_void() 


# Ordenar barras con factores

# cuando necesitamos manipular ordenes en los gráficos de
# variables cualitativas no nos queda otra posibilidad que
# hacerlo mediante el ordenamiento de los niveles de un factor

# tomemos la variable ESTTON (Estrategia del tratamiento)

rita |> 
  count(ESTTON)

# graficamos estos valores en barras horizontales

rita |> 
  filter(!is.na(ESTTON)) |> 
  count(ESTTON) |> 
  ggplot(aes(x = ESTTON, y = n, fill = ESTTON)) + 
  geom_bar(stat = "identity", width = 0.6) + 
  scale_fill_brewer(name = "Estrategia", palette = "Set3") +
  theme(legend.position = "none") +
  geom_text(aes(label = n), nudge_y = 5) +
  coord_flip() +
  xlab(label = "") +
  scale_y_continuous(name = "Frecuencia", 
                     limits = c(0, 265), 
                     breaks = seq(0, 265, by = 10)) +
  theme_minimal()

# utilizamos coord_flip() para cambiar las coordenadas y
# que las barras sean horizontales
# también reducimos el ancho de las barras a 0.6,
# agregamos etiquetas con geom_text() y usamos escalas
# de relleno (fill) de paletas de Brewer (paleta Set3)


# el gráfico tiene categorías desordenadas
# las ordenamos por frecuencia e invertimos - usamos fct_infrec() y
# fct_rev()

rita |> 
  filter(!is.na(ESTTON)) |> 
  mutate(ESTTON = fct_rev(fct_infreq(ESTTON))) |> 
  count(ESTTON) |> 
  ggplot(aes(x = ESTTON, y = n, fill = ESTTON)) + 
  geom_bar(stat = "identity", width = 0.6) + 
  scale_fill_brewer(name = "Estrategia", palette = "Set3") +
  theme(legend.position = "none") +
  geom_text(aes(label = n), nudge_y = 5) +
  coord_flip() +
  xlab(label = "") +
  scale_y_continuous(name = "Frecuencia", 
                     limits = c(0, 265), 
                     breaks = seq(0, 265, by = 10)) +
  theme_minimal()

# Otros paquetes relevantes

# Paquete patchwork

# en ocasiones vamos a necesitar componer gráficos individuales en
# estructuras combinadas

# el paquete pachwork se ocupa de esta tarea de forma sencilla

# almacenemos dos graficos de los realizados anteriormente en objetos
# además de tendencia que ya está guardado

sectores <- rita  |> 
  rstatix::freq_table(PTESXN) |>
  ggplot(aes(x = "", y = n, fill = PTESXN)) +
  geom_bar(stat="identity", width=1) +
  scale_fill_manual(name = "Sexo",
                    values = c("violetred", "dodgerblue2")) +
  geom_text(aes(label = paste0(round(prop, 1), "%")), 
            position = position_stack(vjust = 0.5), color = "white") +
  coord_polar("y", start=90*0.0174533, direction = 1) +
  theme_void() 

estrategias <- rita |> 
  filter(!is.na(ESTTON)) |> 
  mutate(ESTTON = fct_rev(fct_infreq(ESTTON))) |> 
  count(ESTTON) |> 
  ggplot(aes(x = ESTTON, y = n, fill = ESTTON)) + 
  geom_bar(stat = "identity", width = 0.6) + 
  scale_fill_brewer(name = "Estrategia", palette = "Set3") +
  theme(legend.position = "none") +
  geom_text(aes(label = n), nudge_y = 5) +
  coord_flip() +
  xlab(label = "") +
  scale_y_continuous(name = "Frecuencia", 
                     limits = c(0, 265), 
                     breaks = seq(0, 265, by = 10)) +
  theme_minimal()

# compongamos dos gráficos de ejemplo

library(patchwork)

tendencia / estrategias

(tendencia | sectores) / estrategias +
  plot_annotation(tag_levels = "A")

# mediante operadores matemáticos como sumas, 
# cocientes, paréntesis, etc se puede diseñar una
# combinación de gráficos

# además se pueden añadir tags (por ejemplo con letas)

# Paquete ggupset

# en encuentros anteriores conocimos la utilidad del
# gráfico Upset para variables cualitativas con categorías
# que no son mutuamente excluyentes

# existe un paquete que conecta la posibilidad de hacer este
# tipo de gráficos con la posibilidad de hacerlo con ggplot

library(ggupset)

# la librería aporta la capa scale_x_upset()

rita |> 
  filter(!is.na(ESTTON) & ESTTON != "Ignorado") |> 
  group_by(IDPTE) |> 
  summarize(Estrategias = list(ESTTON)) |> 
  ggplot(aes(x=Estrategias)) +
  geom_bar() +
  scale_x_upset(order_by = "degree", n_sets = 6)

# los datos deben de estar en un formato lista agrupados
# por el identificador (usamos IDPTE en este caso por los
# datos con los que contamos en el dataframe)


