library(readr)
library(readxl)
library(dplyr)
library(writexl)

# Importar bases
mapa <- read_csv2("data/mapa.csv")

regioes_ibge <- read_excel("data_raw/regioes_ibge.xlsx")

# Padronizar código IBGE
mapa <- mapa %>%
  mutate(cod_ibge = as.character(cod_ibge))

regioes_ibge <- regioes_ibge %>%
  mutate(cod_ibge = as.character(cod_ibge))

# Juntar nome da RGInt à base
base_junta <- mapa %>%
  left_join(
    regioes_ibge %>%
      select(cod_ibge, nome_rgint),
    by = "cod_ibge"
  )

# Calcular média e P90/P10 por RGInt
tabela_rgint <- base_junta %>%
  group_by(nome_rgint) %>%
  summarise(
    media_valor = mean(valor, na.rm = TRUE),
    p90 = quantile(valor, 0.90, na.rm = TRUE),
    p10 = quantile(valor, 0.10, na.rm = TRUE),
    p90_p10 = p90 / p10,
    .groups = "drop"
  ) %>%
  arrange(nome_rgint)

# Arredondar
tabela_rgint <- tabela_rgint %>%
  mutate(
    media_valor = round(media_valor, 2),
    p90 = round(p90, 2),
    p10 = round(p10, 2),
    p90_p10 = round(p90_p10, 2)
  )

# Visualizar
print(tabela_rgint)

# Exportar
write_xlsx(
  tabela_rgint,
  "data/media_valor_rgint.xlsx"
)


###### Mapa 2

library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

# Importar bases
mapa2025 <- read_csv2("data/mapa2025.csv")

regioes_ibge <- read_excel("data_raw/regioes_ibge.xlsx")

# Padronizar código IBGE
mapa2025 <- mapa2025 %>%
  mutate(cod_ibge = as.character(cod_ibge))

regioes_ibge <- regioes_ibge %>%
  mutate(cod_ibge = as.character(cod_ibge))

# Juntar região intermediária
base_junta <- mapa2025 %>%
  left_join(
    regioes_ibge %>%
      select(cod_ibge, nome_rgint),
    by = "cod_ibge"
  )

# Calcular percentual de municípios em cada quintil por RGInt
tabela_quintis <- base_junta %>%
  filter(!is.na(nome_rgint), !is.na(quintil)) %>%
  count(nome_rgint, quintil) %>%
  group_by(nome_rgint) %>%
  mutate(
    percentual = n / sum(n) * 100
  ) %>%
  ungroup() %>%
  select(nome_rgint, quintil, percentual) %>%
  mutate(
    quintil = paste0(quintil, "º quintil")
  ) %>%
  pivot_wider(
    names_from = quintil,
    values_from = percentual,
    values_fill = 0
  ) %>%
  arrange(nome_rgint)


# Visualizar
print(tabela_quintis)

# Exportar
write_xlsx(
  tabela_quintis,
  "data/quintis_por_rgint_2025.xlsx"
)