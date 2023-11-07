# Encuentro 6
# Resultados: operaciones, estadística descriptiva y exportación 

# activamos tidyverse

library(tidyverse)

# leemos datos

guess_encoding("demoras.csv")

datos <- read_csv2("demoras.csv")


  
datos <- datos |> filter(!is.na(demora1), !is.na(demora2), !is.na(demora3))



datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = mean))

datos <- datos |> 
  filter(demora1 >= 0, demora2 >=0, demora3 >= 0)

datos <- datos |> 
  filter(demora1 < 2000, demora2 < 2000, demora3 < 2000)

datos <- datos |> sample_n(size = 1000, replace = F)

# across()

datos |> 
  summarise(media_demora1 = mean(demora1),
            media_demora2 = mean(demora2),
            media_demora3 = mean(demora3))


datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = mean))


datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = c(mean, min, max)))


datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = list(media = mean, min = min, max = max), 
                   .names = "{.col}_{.fn}"))

# rowwise()

datos |> 
  group_by(IDPTE)

datos |> 
  rowwise()

datos |> 
    mutate(sumatoria = demora1 + demora2 + demora3)

datos |> 
  rowwise() |> 
  mutate(sumatoria = sum(c(demora1, demora2, demora3)))

# ver de agregar NA`s`


# c_across()

datos |> 
  rowwise() |> 
  mutate(sumatoria = sum(c_across(demora1:demora3)))


# rstatix
# get_summary_stats()

library(rstatix)

datos |> 
  select(starts_with("demora")) |> 
  get_summary_stats(type = "common")

datos |> 
  select(starts_with("demora")) |> 
  get_summary_stats(type = "robust")

datos |> 
  select(starts_with("demora")) |> 
  get_summary_stats(type = "full")

# get_mode()

datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = get_mode))


datos |> 
  filter(demora1 > 0, demora2 > 0, demora3 > 0) |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = get_mode))


# freq_table()

datos |> 
  freq_table(PTESXN)

# gtsummary

library(gtsummary)

datos |> 
  tbl_summary()
    

datos |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary()

datos |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(statistic = list(
    all_continuous() ~ "{mean} ({sd})",
    all_categorical() ~ "{n} / {N} ({p}%)"
  ),
  label = PTESXN ~ "Sexo")

datos |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(by = PTESXN) |> 
  modify_header(label ~ "**Variable**") |> 
  bold_labels()

# Etiquetado - labelled

library(labelled)

datos_etiquetados <- datos |> 
  set_variable_labels(PTESXN = "Sexo", 
                      demora1 = "Demora Sintomas-1raConsulta", 
                      demora2 = "Demora 1raConsulta-Diagnostico", 
                      demora3 = "Demora Diagnostico-Tratamiento") 

datos_etiquetados |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(by = PTESXN) |> 
  modify_header(label ~ "**Variable**") |> 
  bold_labels()

look_for(datos_etiquetados)

datos_etiquetados <- datos_etiquetados |> 
  set_value_labels(PTESXN = c(Varon = "Hombre", Mujer = "Mujer")) 


look_for(datos_etiquetados)


# Exportar con flextable

frec_sexo <- datos |> 
  freq_table(PTESXN) |> 
  flextable()

resumen_demora <- datos_etiquetados |> 
  select(starts_with("demora"), PTESXN) |> 
  tbl_summary(by = PTESXN) |> 
  modify_header(label ~ "**Variable**") |> 
  bold_labels() |> 
  as_flex_table()

save_as_docx("tabla 1" = frec_sexo, 
             "tabla 2" = resumen_demora, 
             path = "practicas/tablas.docx")






