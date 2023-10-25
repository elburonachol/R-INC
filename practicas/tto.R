library(tidyverse)

guess_encoding("practicas/DATOSTRATAMIENTOS-TRATAMIENTOS-2023926.csv")

tto <- read_csv2("practicas/DATOSTRATAMIENTOS-TRATAMIENTOS-2023926.csv",
                 locale = locale(encoding = "ISO-8859-1"), na = c("", "N/D"))

glimpse(tto)

tto <- tto |>  mutate(FITTO = dmy(FITTO),
                      FINTO = dmy(FINTO))

summary(tto$FITTO)

tto |> 
  count(IDPTE, IDTUM, IDTTO, sort = T)

tto |> 
  count(IDPTE, IDTUM, sort = T)

tto |> 
  count(IDPTE, sort = T)

idtum <- tto |> 
  count(IDPTE, IDTUM) |> 
  filter(n > 1) |>  pull(IDTUM)

trat <- tto |>  filter(IDTUM %in% idtum)

summary(trat$FITTO)

trat |> 
  select(IDPTE, IDTUM, IDTTO, FITTO, FINTO, ESTTON, TPGFN) |> 
  arrange(IDPTE, IDTUM, IDTTO)

# cantidad de estrategias por tumor/paciente

tto |> 
  distinct(ESTTON)

tto |> 
  select(IDPTE, IDTUM, IDTTO, FITTO, FINTO, ESTTON, TPGFN) |> 
  filter(!is.na(ESTTON) & ESTTON != "Ignorado") |> 
  count(IDPTE, IDTUM, ESTTON) |> 
  pivot_wider(names_from = ESTTON, values_from = n, values_fill = 0) |> 
  View()

# tiempos promedio de cada tto segun topografia
# antes se debería agrupar la topografia

tto |> 
  filter(FITTO > "1900-01-01" & FINTO > "1900-01-01") |> 
  mutate(tiempo = interval(FITTO, FINTO)/ddays(),
         año_inicio = year(FITTO),
         mes_inicio = month(FITTO)) |> 
  select(IDPTE, IDTUM, IDTTO, tiempo, año_inicio, mes_inicio, ESTTON, TPGFN) |> 
  filter(!is.na(tiempo) & tiempo >= 0 & !is.na(ESTTON)) |> 
  group_by(TPGFN, ESTTON) |> 
  summarise(tiempo = mean(tiempo)) |> 
  arrange(desc(tiempo))



tto |> 
  mutate(tiempo = interval(FITTO, FINTO)/ddays(),
         año_inicio = year(FITTO),
         mes_inicio = month(FITTO),
         inicio = str_glue("{año_inicio}-{mes_inicio}")) |> 
  select(IDPTE, IDTUM, IDTTO, tiempo, año_inicio, mes_inicio, inicio, ESTTON, TPGFN) |> 
  filter(!is.na(tiempo) & tiempo >= 0 & !is.na(ESTTON) & año_inicio == 2023) |> 
  arrange(inicio) |> 
  pivot_wider(names_from = inicio, values_from = ESTTON) |> View()

## tabla segun Gisel

tabla <- tto |> 
  filter(!is.na(ESTTON) & ESTTON != "Ignorado") |> 
  select(IDPTE, IDTUM, FITTO, ESTTON, TPGFN) |> 
  group_by(IDPTE, IDTUM, TPGFN, ESTTON) |> 
  filter(FITTO == min(FITTO)) |> 
  ungroup() |> 
  distinct(IDPTE, IDTUM, FITTO, ESTTON, TPGFN) |>
  pivot_wider(names_from = ESTTON, values_from = FITTO)


## 
  
tto |> 
  filter(!is.na(ESTTON), ESTTON != "Ignorado", !is.na(FITTO)) |> 
  select(IDPTE, IDTUM, FITTO, ESTTON, TPGFN) |> 
  group_by(IDPTE, IDTUM, TPGFN, ESTTON) |> 
  mutate(trat = min_rank(FITTO)) |> 
  ungroup() |> 
  distinct(IDPTE, IDTUM, FITTO, ESTTON, TPGFN, trat) |>
  pivot_wider(names_from = trat, values_from = FITTO) 
