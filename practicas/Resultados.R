# Encuentro 6
# Resultados: operaciones, estadística descriptiva y exportación 

# activamos tidyverse

library(tidyverse)

# leemos datos

datos <- read_csv2("demoras.csv")
  
datos <- datos |> filter(!is.na(demora1), !is.na(demora2), !is.na(demora3))



datos |> 
  summarise(across(.cols =  starts_with("demora"), 
                   .fns = mean))

# across()




# rowwise()



# c_across()


# rstatix
# get_summary_stats()


# get_mode()


# counts_to_cases()


# gtsummary


# Etiquetado - labelled


# Exportar con flextable






# freq_table()

