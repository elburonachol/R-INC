library(tidyverse)

tto <- read_csv2("practicas/DATOSTRATAMIENTOS-TRATAMIENTOS-2023926.csv",
                 locale = locale(encoding = "ISO-8859-1"), na = c("", "N/D"))

rita <- tto |> 
  select(IDPTE, IDTUM, FEINI, FECON, PTESXN, TPGFN, MFGN)

tratamiento <- tto |> 
  select(IDTUM, IDTTO, FITTO, INTTON, ESTTON, FINTO, RTATON)

tratamiento |> count(IDTUM, IDTTO, sort = T)

para_filtrar <- tratamiento |> count(IDTUM, sort = T) |> 
  filter(n > 5)

tratamiento <- tratamiento |> anti_join(para_filtrar)

tratamiento <- tratamiento |> slice_head(n = 500)

para_filtrar <- tratamiento |> distinct(IDTUM)

rita <- rita |> distinct(IDPTE, IDTUM, .keep_all = T) |> semi_join(para_filtrar)

rita |> count(IDTUM, sort = T)

write_csv2(rita, "practicas/rita_sin_tratamiento.csv")

write_csv2(tratamiento, "practicas/tratamientos.csv")


###

rita_sin_tratatamiento <- read_csv2("practicas/rita_sin_tratamiento.csv")

tratamientos <- read_csv2("practicas/tratamientos.csv")

library(janitor)



get_dupes(rita_sin_tratatamiento)

## quitamos duplicado con distinct()

rita_sin_tratatamiento <- rita_sin_tratatamiento |> 
  distinct()

get_dupes(rita_sin_tratatamiento)

##quitamos duplicado con slice_

get_dupes(tratamientos, IDTUM, IDTTO)

tratamientos <- tratamientos |> group_by(IDTUM, IDTTO) |> 
  slice_min(FITTO) |> ungroup()

get_dupes(tratamientos, IDTUM, IDTTO)

# unión

rita_completo <- rita_sin_tratatamiento |> 
  inner_join(tratamientos, by = "IDTUM") 

get_dupes(rita_completo, IDPTE, IDTUM, IDTTO)

## pivoteo

rita_completo <- rita_completo |> 
  mutate(FITTO = dmy(FITTO),
         FINTO = dmy(FINTO))

tto_ancho <- rita_completo |> 
  select(-IDTTO) |> 
  filter(!is.na(FITTO)) |> 
  group_by(IDPTE, IDTUM, TPGFN, MFGN, FEINI, FECON, INTTON, ESTTON) |> 
  mutate(trat = min_rank(FITTO),
         trat = str_glue("trat{trat}")) |> 
  ungroup() |> 
  arrange(trat) |> 
  pivot_wider(names_from = trat, 
              names_glue = "{trat}_{.value}", 
              values_from = c(FITTO, FINTO))

tto_ancho |> 
  filter(!is.na(ESTTON)) |> 
  mutate(tiempo_trat1 = interval(trat1_FITTO, trat1_FINTO)/ddays(),
         tiempo_trat2 = interval(trat2_FITTO, trat2_FINTO)/ddays(),
         tiempo_trat3 = interval(trat3_FITTO, trat3_FINTO)/ddays()) |> 
  filter(tiempo_trat1 >= 0) |> 
  group_by(ESTTON) |> 
  summarise(media_trat1 = mean(tiempo_trat1, na.rm = T),
            n_trat1 = n())

rita_completo |> 
  filter(!is.na(ESTTON), !is.na(FITTO), !is.na(FINTO),
         year(FITTO) > 2009, year(FINTO) > 2009, FINTO >= FITTO) |> 
  mutate(tiempo = interval(FITTO, FINTO)/ddays()) |> 
  group_by(INTTON, ESTTON) |> 
  summarise(media = mean(tiempo, na.rm = T),
            mediana = median(tiempo, na.rm = T),
            cantidad = n())

# igual con skim

library(skimr)

rita_completo |> 
  filter(!is.na(ESTTON), !is.na(FITTO), !is.na(FINTO),
         year(FITTO) > 2009, year(FINTO) > 2009,
         FINTO >= FITTO) |> 
  mutate(tiempo = interval(FITTO, FINTO)/ddays()) |> 
  group_by(INTTON, ESTTON) |> 
  skim(tiempo) |> 
  select(-skim_variable, -skim_type, -n_missing, -complete_rate)

library(UpSetR)

tabla <- rita_completo |> 
  select(IDPTE, IDTUM, IDTTO, FITTO, FINTO, ESTTON, TPGFN) |> 
  filter(!is.na(ESTTON) & ESTTON != "Ignorado") |> 
  count(IDPTE, IDTUM, ESTTON) |> 
  mutate(n = if_else(n > 0, 1, 0)) |> 
  pivot_wider(names_from = ESTTON, values_from = n , values_fill = 0) |> 
  select(-IDPTE)

tabla <- as.data.frame(tabla)

upset(tabla)
