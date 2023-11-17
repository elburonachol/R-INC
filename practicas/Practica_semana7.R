# Esta practica pertenece al temario del Encuentro 7 del curso

# La idea es ver algunas otras funciones o aplicaciones de lo
# mostrado durante el encuentro sincrónico

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

# almacenamos el gráfico en el objeto tendencia

tendencia

# luego podemos ir agregando caracteristicas y capas al objeto

# por ejemplo, agregamos escalas de ejes x e y

tendencia +
  scale_x_continuous(name = "Año", breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(name = "Frecuencia", limits = c(0,150), 
                     breaks = seq(0,150, by = 10))

# ahora le podemos incorporar etiquetas (de titulo, subtitulo y pie)

tendencia <- tendencia +
  scale_x_continuous(name = "Año", breaks = seq(2012, 2023, by = 1)) +
  scale_y_continuous(name = "Frecuencia", limits = c(0,150), 
                     breaks = seq(0,150, by = 10)) +
  labs(title = "Número de registros en RITA por año", 
       subtitle = "Argentina - 2012-2023" , 
       caption = "Fuente: SIVER-Ca en base a datos del RITA. INC")

tendencia

# además podemos cambiar el tema, ubicar la leyenda debajo y sin titulo,
# y cambiar colores

tendencia <- tendencia +
  scale_color_manual(values = c("royalblue3", "peru", "forestgreen")) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title = element_blank())

tendencia

# estos es lo mismo que construirlo todo junto

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


# personalizar temas gráficos

# podemos especificar ciertas caracteristicas generales de theme
# como tipo de fuentes, tamaños, etc

# la librería systemfonts tiene acceso a las fuentes nativas del S.O

library(systemfonts)

# mostramos las fuentes instaladas
system_fonts() |> View()

# una fuente habitualmente instalada es Times New Roman

system_fonts() |> 
  filter(family == "Times New Roman")

# guardamos las características del tema que usamos 

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

# los cambios en el tema desaparecen cuando iniciamos una sesión de R
# nueva o bien podemos recuperar el tema minimal original almacenado

theme_set(backup_tema)

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

# 


# lineas longitudinales  con rita ( ggpubr::ggtexttable() )

# start de coord_polar() en grafico de tortas

# annotate

# ordenar con factores

# ggupset, ggrepel, ggtext, patchwork, rcartocolor, systemfonts, scales

# ver cambio de fuentes y tamaños
# caracteristicas de temas


