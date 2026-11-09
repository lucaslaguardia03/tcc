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

#Colunas variação absoluta e média da rgint - tabela 5

library(readr)
library(readxl)
library(dplyr)
library(writexl)

# Importar bases
dadosx <- read_excel("data/base_final.xlsx") %>%
  filter(ano %in% c(2019, 2025))

# Variação absoluta municipal do vam_fundeb (2025 - 2019), em R$ de 2025 por matrícula
dadosx <- dadosx %>%
  group_by(cod_ibge) %>%
  mutate(variacao = vam_fundeb[ano == 2025] - vam_fundeb[ano == 2019]) %>%
  ungroup()

regioes_ibge <- read_excel("data_raw/regioes_ibge.xlsx")

# Padronizar código IBGE
dadosx <- dadosx %>%
  mutate(cod_ibge = as.character(cod_ibge))

regioes_ibge <- regioes_ibge %>%
  mutate(cod_ibge = as.character(cod_ibge))

# Juntar nome da RGInt à base
base_junta <- dadosx %>%
  filter(ano %in% c(2019, 2025)) %>%
  left_join(regioes_ibge, by = "cod_ibge") %>%
  select(cod_ibge, nom_ente, ano, variacao, nome_rgint, mat_censo, vam_fundeb)

# Média RGInt (%): crescimento do VAM da região como um todo (ponderado pelas matrículas)
# x1 = recursos de 2025 / matrículas de 2025; x2 = o mesmo para 2019; crescimento = x1/x2 - 1
crescimento_rgint <- base_junta %>%
  group_by(nome_rgint) %>%
  summarise(
    x1 = sum(vam_fundeb[ano == 2025] * mat_censo[ano == 2025]) / sum(mat_censo[ano == 2025]),
    x2 = sum(vam_fundeb[ano == 2019] * mat_censo[ano == 2019]) / sum(mat_censo[ano == 2019]),
    media_rgint_pct = (x1 / x2 - 1) * 100,
    .groups = "drop"
  ) %>%
  select(nome_rgint, media_rgint_pct)

# Variação absoluta municipal (R$): média e mediana entre os municípios de cada região
variacao_rgint <- base_junta %>%
  filter(ano == 2025) %>%                 # uma linha por município
  group_by(nome_rgint) %>%
  summarise(
    variacao_abs_media_mun   = mean(variacao),
    variacao_abs_mediana_mun = median(variacao),
    .groups = "drop"
  )

resultado <- left_join(crescimento_rgint, variacao_rgint, by = "nome_rgint")

write_xlsx(resultado, "data/tabela 5 colunas extras.xlsx")


# VAM da RGInt em 2025 (recursos deflacionados / matrículas da região)
vam_rgint_2025 <- base_junta %>%
  filter(ano == 2025) %>%
  group_by(nome_rgint) %>%
  summarise(
    vam_rgint_2025 = sum(vam_fundeb * mat_censo) / sum(mat_censo),
    .groups = "drop"
  )

write_xlsx(vam_rgint_2025, "data/vam regiao 2025.xlsx")