## Boxplot com e sem outliers

library(dplyr)
library(readxl)

com_outliers <- read_excel("data/boxplot4.2.xlsx") %>%
  select(ano, vaa_fundeb, cod_ibge) %>%
  mutate(
    ano = as.numeric(ano),
    cod_ibge = as.character(cod_ibge)
  )

sem_outliers <- read_excel("data/base_sem_outliers_codibge.xlsx") %>%
  select(ano, sem_outliers = vaa_fundeb, cod_ibge) %>%
  mutate(
    ano = as.numeric(ano),
    cod_ibge = as.character(cod_ibge)
  ) %>%
  distinct(ano, cod_ibge, .keep_all = TRUE)

base_boxplot <- com_outliers %>%
  left_join(
    sem_outliers,
    by = c("ano", "cod_ibge")
  )


writexl::write_xlsx(
  base_boxplot,
  "data/boxplot_completo_outliers.xlsx"
)