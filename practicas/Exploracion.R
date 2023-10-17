## Exploracion y diagnóstico

# Activamos paquetes

library(tidyverse)
library(skimr)
library(dlookr)

# Leemos tabla de datos

rita <- read_csv2("practicas/rita_prueba.csv", 
                  locale = locale(encoding = "UTF-8"))

# Exploración de estructura básico

glimpse(rita)

# Exploración usando skim

skim(rita)

# Si tenemos problemas en la visualización 
# de los histogramas de las variables numéricas,
# podemos ejecutar la siguiente función:

fix_windows_histograms()

# y volver a pedir el resumen del dataframe

skim(rita)

# Exploración de variables individuales
# por ejemplo, la edad al diagnóstico

skim(rita, DGED)

####

# Diagnóstico general con dlookr

diagnose(rita)

# Diagnóstico de variables de formato caracter 

diagnose_category(rita)

# Diagnóstico de variables de formato numérico

diagnose_numeric(rita)

# Diagnóstico de oulier

diagnose_outlier(rita)

# Diagnóstico gráfico de outlier

rita |> 
    plot_outlier(DGED)

# Diagnóstico de valores perdidos

find_na(rita, rate = T)

# Diagnóstico gráfico de valores perdidos

plot_na_pareto(rita)

#####

# Selección de variables / columnas

rita |> 
  select(starts_with("ID"))


rita |> 
  select(ends_with("N"))


rita |> 
  select(starts_with("ID") & ends_with("N"))


rita |> 
  select(contains("TTO"))

# combinando select() con skim

# seleccionamos todas las variables de formato numérico y
# que no comiencen con "ID"

rita |> 
  select(where(is.numeric) & !starts_with("ID")) |> 
  skim()

# combinando con dlookr

rita |> 
  select(1:10) |> 
  plot_na_pareto()

rita |> 
  select()


####

# Filtrado de observaciones por condición

rita |> 
  filter(PTESXN == "Mujer")

rita |> 
  filter(DGED > 80)

# combinando select() y filter()

rita |> 
  filter(DGED > 80) |> 
  select(IDPTE, DGED)

# combinando lo aprendido en piezas diferentes

rita |> 
  select(where(is.numeric) & !starts_with("ID"))  |> 
  skim() |> 
  filter(n_missing > 0)


rita |> 
  select(!where(is.logical)) |> 
  diagnose() |> 
  select(-unique_count, -unique_rate) |> 
  filter(missing_count > 0)
