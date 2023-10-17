## Transformación de datos con dplyr

# Activamos paquete

library(tidyverse)


# Leemos tabla de datos

rita <- read_csv2("practicas/rita_prueba.csv", 
                  locale = locale(encoding = "UTF-8"))


# Ordenar con arrange()

# Ordenamos por ID del paciente (forma ascendente)

rita |> 
  arrange(IDPTE)

# Invertimos orden (forma descendente)

rita |> 
  arrange(desc(IDPTE))

# Ordenamos primero por IDESTN y luego por IDPTE

rita |> 
  arrange(IDESTN, IDPTE) |> 
  select(IDESTN, IDPTE)

# Crear nuevas variables con mutate()

# Transformamos la escala de la variable DGED mantenimendo la variable
# original

rita |> 
  mutate(DGED_log = log(DGED)) |> 
  select(IDPTE, DGED, DGED_log)

# lo mismo pero sobreescribimos la variable

rita |> 
  mutate(DGED = log(DGED)) |> 
  select(IDPTE, DGED)


# Resumir variables con summarise()

# Resumimos la variable DGED calculando su mediana

rita |> 
  summarise(mediana_DGED = median(DGED))

# Cantidad de categorías diferentes de IDESTN (agregamos 
# función n_distinct)

rita |> 
  summarise(x = n_distinct(IDESTN))

# Estratificar o agrupar con group_by()

rita |> 
  group_by(PTESXN)

# vemos los metadatos del dataframe 

# Combinando con summarise()

rita |> 
  group_by(PTESXN) |> 
  summarise(mediana_DGED = median(DGED))

# agregamos cantidad de cada categoría (agregamos función n)

rita |> 
  group_by(PTESXN) |> 
  summarise(mediana_DGED = median(DGED),
            cantidad = n())

# combinamos dos agrupamientos

rita |> 
  group_by(PTEDU, PTESXN) |> 
  summarise(mediana_DGED = median(DGED),
            cantidad = n()) |> 
  filter(cantidad > 1) |> 
  View()

# metadatos permanentes

tabla1 <- rita |> 
  group_by(PTEDU, PTESXN) |> 
  summarise(mediana_DGED = median(DGED),
            cantidad = n()) |> 
  filter(cantidad > 1)

tabla1  # visualizar metadatos

tabla1 |> summarise(x = sum(cantidad))

tabla1 <- tabla1 |>  ungroup()

tabla1

tabla1 |> summarise(x = sum(cantidad))

# alternativa con argumento .by dentro de summarise()

rita |> 
  summarise(mediana_DGED = median(DGED),
            cantidad = n(),
            .by = c(PTEDU, PTESXN)) |> 
  filter(cantidad > 1)

# Creando tablas de frecuencia absoluta con count()

rita |> 
  count(PTESXN)

# combinando con más variables y agregando ordenamiento

rita |> 
  count(PTEDU, PTESXN, sort = T)

# Condicional if_else()

categ_educ <- c("Se Ignora", "N/D")

rita |> 
  mutate(Educacion = if_else(PTEDU %in% categ_educ, "Sin datos", PTEDU)) |> 
  count(Educacion)


# Condicional case_when()

rita |> 
  mutate(DGED_categorica = case_when(
    DGED < 10 ~ "0-9",
    DGED >= 10 & DGED < 20 ~ "10-19",
    DGED >= 20 & DGED < 35 ~ "20-34",
    DGED >= 35 & DGED < 50 ~ "35-49",
    DGED >= 50 & DGED < 65 ~ "50-64",
    DGED >= 65 ~ "65 y más",
  )) |> 
  count(DGED_categorica)

# Condicional between()

rita |> 
  mutate(DGED_categorica = case_when(
    between(DGED, 0, 9) ~ "0-9",
    between(DGED, 10, 19) ~ "10-19",
    between(DGED, 20, 34) ~ "20-34",
    between(DGED, 35, 49) ~ "35-49",
    between(DGED, 50, 64) ~ "50-64",
    between(DGED, 65, Inf) ~ "65 y más",
  )) |> 
  count(DGED_categorica)

# Combinando piezas

rita |> slice_max(DGED, prop = 0.01) |> select(DGED)

# incluir slice() y/o head()  
# slice_head() y slice_max()
# incluir relocate()


