library(readxl)
library(dplyr)
library(writexl)

# Ler as bases
base_nse <- read_excel("data_raw/NSE-DRec.xlsx")
base_renda <- read_excel("data_raw/tabela10295.xlsx")

# Padronizar cod_ibge como character
base_nse <- base_nse %>%
  mutate(cod_ibge = as.character(cod_ibge))

base_renda <- base_renda %>%
  mutate(cod_ibge = as.character(cod_ibge))

# Juntar VL da tabela 10295 pela chave cod_ibge
base_nse <- base_nse %>%
  left_join(
    base_renda %>%
      select(cod_ibge, VL),
    by = "cod_ibge"
  )

# Criar vintis de renda e calcular os indicadores por vintil
base_grafico <- base_nse %>%
  filter(!is.na(VL),
         !is.na(`VAM com NSE e Drec`),
         !is.na(`VAM sem NSE e Drec`)) %>%
  mutate(
    vintil = ntile(VL, 20)
  ) %>%
  group_by(vintil) %>%
  summarise(
    renda_per_capita_media = mean(VL, na.rm = TRUE),
    VAM_sem_NSE_Drec = mean(`VAM sem NSE e Drec`, na.rm = TRUE),
    VAM_com_NSE_Drec = mean(`VAM com NSE e Drec`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    variacao_VAM = (VAM_com_NSE_Drec / VAM_sem_NSE_Drec - 1) * 100
  )

# Exportar para Excel
write_xlsx(
  base_grafico,
  "data/base_grafico_vintis.xlsx"
)


