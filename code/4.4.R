library(dplyr)
library(readxl)
library(openxlsx)

# Importar bases
base_final <- read_excel("data/base_final.xlsx")

regioes_ibge <- read_excel("data_raw/regioes_ibge.xlsx") %>%
  select(cod_ibge, nome_rgint)

# Mapa de beneficiários líquidos por matrícula - 2018
# Valores corrigidos pelo IPCA
mapa_beneficiarios_18 <- base_final %>%
  filter(ano == 2018) %>%
  mutate(
    saldo = ((rectot_fundeb - contrib_fundeb) * defla_ipca) / mat_censo
  ) %>%
  select(cod_ibge, saldo) %>%
  left_join(regioes_ibge, by = "cod_ibge") %>%
  select(cod_ibge, saldo, nome_rgint)

# Mapa de beneficiários líquidos por matrícula - 2025
mapa_beneficiarios_25 <- base_final %>%
  filter(ano == 2025) %>%
  mutate(
    saldo = (rectot_fundeb - contrib_fundeb) / mat_censo
  ) %>%
  select(cod_ibge, saldo) %>%
  left_join(regioes_ibge, by = "cod_ibge") %>%
  select(cod_ibge, saldo, nome_rgint)

# Análise regional de contribuintes e beneficiários líquidos - 2025
analise_regional_25 <- base_final %>%
  filter(ano == 2025) %>%
  mutate(
    saldo = rectot_fundeb - contrib_fundeb,
    classificacao = case_when(
      saldo < 0 ~ "Contribuinte líquido",
      saldo > 0 ~ "Beneficiário líquido",
      TRUE ~ "Saldo zero"
    )
  ) %>%
  left_join(
    regioes_ibge %>% select(cod_ibge, nome_rgint),
    by = "cod_ibge"
  ) %>%
  group_by(nome_rgint) %>%
  summarise(
    contribuintes_liquidos = sum(
      classificacao == "Contribuinte líquido",
      na.rm = TRUE
    ),
    beneficiarios_liquidos = sum(
      classificacao == "Beneficiário líquido",
      na.rm = TRUE
    ),
    saldo_zero = sum(
      classificacao == "Saldo zero",
      na.rm = TRUE
    ),
    total_municipios = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(beneficiarios_liquidos))

# Exportar cada base para um arquivo CSV separado

write.csv2(
  mapa_beneficiarios_18,
  file = "data/mapa_beneficiarios_18.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv2(
  mapa_beneficiarios_25,
  file = "data/mapa_beneficiarios_25.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv2(
  analise_regional_25,
  file = "data/analise_regional_25.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)


# Análise das mudanças de classificação entre 2018 e 2025

classificacao_municipios <- base_final %>%
  filter(ano %in% c(2018, 2025)) %>%
  mutate(
    saldo = rectot_fundeb - contrib_fundeb,
    classificacao = case_when(
      saldo < 0 ~ "Contribuinte líquido",
      saldo > 0 ~ "Beneficiário líquido",
      TRUE ~ "Saldo zero"
    )
  ) %>%
  select(cod_ibge, ano, classificacao) %>%
  distinct() %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = classificacao,
    names_prefix = "classificacao_"
  ) %>%
  left_join(
    regioes_ibge %>% select(cod_ibge, nome_rgint),
    by = "cod_ibge"
  ) %>%
  mutate(
    transicao = case_when(
      classificacao_2018 == "Contribuinte líquido" &
        classificacao_2025 == "Beneficiário líquido" ~
        "Contribuinte → Beneficiário",
      
      classificacao_2018 == "Beneficiário líquido" &
        classificacao_2025 == "Contribuinte líquido" ~
        "Beneficiário → Contribuinte",
      
      classificacao_2018 == "Contribuinte líquido" &
        classificacao_2025 == "Contribuinte líquido" ~
        "Permaneceu contribuinte",
      
      classificacao_2018 == "Beneficiário líquido" &
        classificacao_2025 == "Beneficiário líquido" ~
        "Permaneceu beneficiário",
      
      classificacao_2018 == "Saldo zero" &
        classificacao_2025 != "Saldo zero" ~
        "Saiu do saldo zero",
      
      classificacao_2018 != "Saldo zero" &
        classificacao_2025 == "Saldo zero" ~
        "Passou a saldo zero",
      
      classificacao_2018 == "Saldo zero" &
        classificacao_2025 == "Saldo zero" ~
        "Permaneceu com saldo zero",
      
      TRUE ~ "Sem classificação completa"
    )
  )

# 1. Detalhamento por município: identificar quem mudou de situação
transicoes_municipios <- classificacao_municipios %>%
  filter(
    transicao %in% c(
      "Contribuinte → Beneficiário",
      "Beneficiário → Contribuinte"
    )
  ) %>%
  arrange(nome_rgint, transicao, cod_ibge)

# 2. Contagem das transições por região intermediária
transicoes_regiao <- classificacao_municipios %>%
  group_by(nome_rgint) %>%
  summarise(
    contribuintes_2018 = sum(
      classificacao_2018 == "Contribuinte líquido",
      na.rm = TRUE
    ),
    beneficiarios_2018 = sum(
      classificacao_2018 == "Beneficiário líquido",
      na.rm = TRUE
    ),
    contribuintes_2025 = sum(
      classificacao_2025 == "Contribuinte líquido",
      na.rm = TRUE
    ),
    beneficiarios_2025 = sum(
      classificacao_2025 == "Beneficiário líquido",
      na.rm = TRUE
    ),
    contribuintes_viraram_beneficiarios = sum(
      transicao == "Contribuinte → Beneficiário",
      na.rm = TRUE
    ),
    beneficiarios_viraram_contribuintes = sum(
      transicao == "Beneficiário → Contribuinte",
      na.rm = TRUE
    ),
    permaneceram_contribuintes = sum(
      transicao == "Permaneceu contribuinte",
      na.rm = TRUE
    ),
    permaneceram_beneficiarios = sum(
      transicao == "Permaneceu beneficiário",
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  arrange(
    desc(contribuintes_viraram_beneficiarios)
  )

# 3. Exportar em CSV
write.csv2(
  transicoes_municipios,
  "data/transicoes_municipios_2018_2025.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv2(
  transicoes_regiao,
  "data/transicoes_regiao_2018_2025.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)




#--------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readxl)

# Importar bases
base_final <- read_excel("data/base_final.xlsx")

regioes_ibge <- read_excel("data_raw/regioes_ibge.xlsx") %>%
  select(cod_ibge, nome_rgint)

# ============================================================
# 1. CLASSIFICAR MUNICÍPIOS EM QUINTIS POR RECEITA POR MATRÍCULA
# ============================================================

quintis_municipios <- base_final %>%
  filter(ano %in% c(2018, 2025)) %>%
  mutate(
    fundeb_por_matricula = if_else(
      !is.na(mat_censo) & mat_censo > 0,
      rectot_fundeb / mat_censo,
      NA_real_
    )
  ) %>%
  filter(!is.na(fundeb_por_matricula)) %>%
  select(cod_ibge, ano, fundeb_por_matricula) %>%
  group_by(ano) %>%
  mutate(
    quintil = ntile(fundeb_por_matricula, 5)
  ) %>%
  ungroup() %>%
  select(cod_ibge, ano, quintil) %>%
  pivot_wider(
    names_from = ano,
    values_from = quintil,
    names_prefix = "quintil_"
  ) %>%
  filter(
    !is.na(quintil_2018),
    !is.na(quintil_2025)
  ) %>%
  left_join(regioes_ibge, by = "cod_ibge") %>%
  mutate(
    evolucao = case_when(
      quintil_2025 > quintil_2018 ~ "Melhoraram",
      quintil_2025 < quintil_2018 ~ "Diminuíram",
      TRUE ~ "Permaneceram"
    )
  )

# ============================================================
# 2. SÍNTESE DA MOBILIDADE ENTRE QUINTIS POR REGIÃO
# ============================================================

analise_quintis_regiao <- quintis_municipios %>%
  group_by(nome_rgint) %>%
  summarise(
    total_municipios = n(),
    melhoraram = sum(evolucao == "Melhoraram"),
    permaneceram = sum(evolucao == "Permaneceram"),
    diminuiram = sum(evolucao == "Diminuíram"),
    .groups = "drop"
  ) %>%
  mutate(
    perc_melhoraram = round(
      100 * melhoraram / total_municipios, 1
    ),
    perc_permaneceram = round(
      100 * permaneceram / total_municipios, 1
    ),
    perc_diminuiram = round(
      100 * diminuiram / total_municipios, 1
    )
  ) %>%
  arrange(desc(perc_melhoraram))

# Visualizar tabela
analise_quintis_regiao

# Exportar síntese regional
write.csv2(
  analise_quintis_regiao,
  "data/analise_quintis_regiao_2018_2025.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

# ============================================================
# 3. MATRIZ DE TRANSIÇÃO ENTRE QUINTIS - NÚMEROS ABSOLUTOS
# ============================================================

matriz_transicao <- quintis_municipios %>%
  count(quintil_2018, quintil_2025, name = "municipios") %>%
  complete(
    quintil_2018 = 1:5,
    quintil_2025 = 1:5,
    fill = list(municipios = 0)
  ) %>%
  pivot_wider(
    names_from = quintil_2025,
    values_from = municipios,
    names_prefix = "Q_2025_"
  ) %>%
  arrange(quintil_2018) %>%
  rename(Q_2018_origem = quintil_2018)

# Visualizar matriz absoluta
matriz_transicao

# ============================================================
# 4. MATRIZ DE TRANSIÇÃO - PERCENTUAIS POR QUINTIL DE ORIGEM
# ============================================================

matriz_transicao_perc <- quintis_municipios %>%
  count(quintil_2018, quintil_2025, name = "municipios") %>%
  group_by(quintil_2018) %>%
  mutate(
    percentual = round(
      100 * municipios / sum(municipios), 1
    )
  ) %>%
  ungroup() %>%
  select(-municipios) %>%
  complete(
    quintil_2018 = 1:5,
    quintil_2025 = 1:5,
    fill = list(percentual = 0)
  ) %>%
  pivot_wider(
    names_from = quintil_2025,
    values_from = percentual,
    names_prefix = "Q_2025_"
  ) %>%
  arrange(quintil_2018) %>%
  rename(Q_2018_origem = quintil_2018)

# Visualizar matriz percentual
matriz_transicao_perc

# ============================================================
# 5. EXPORTAR MATRIZES DE TRANSIÇÃO
# ============================================================

write.csv2(
  matriz_transicao,
  "data/matriz_transicao_quintis_2018_2025.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

write.csv2(
  matriz_transicao_perc,
  "data/matriz_transicao_quintis_perc_2018_2025.csv",
  row.names = FALSE,
  fileEncoding = "UTF-8"
)