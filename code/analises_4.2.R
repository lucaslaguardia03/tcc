##Análises 4.2

library(e1071)
library(readODS)
library(dplyr)
library(data.table)
library(readxl)
library(openxlsx)
library(ineq)
library(tidyr)

base_final <- readxl::read_excel("data/base_final.xlsx")
boxplot42 <- readxl::read_excel("data/boxplot4.2.xlsx")

#1 - Índice de Gini da distribuição dos recursos totais do Fundeb entre os municípios de MG, por ano

base_final <- base_final %>% mutate(vaa_fundeb = ((rectot_fundeb * defla_ipca) /mat_censo))

gini_fundeb_ano <- base_final %>% group_by(ano) %>%  summarize(gini = Gini(vaa_fundeb), .groups = "drop")


#1.5 - Análise estatística do Fundeb entre os municípios de MG, por ano


indicadores_ano <- base_final %>%
  group_by(ano) %>%
  summarise(
    Média = mean(vaa_fundeb, na.rm = TRUE),
    Mediana = median(vaa_fundeb, na.rm = TRUE),
    `Desvio padrão` = sd(vaa_fundeb, na.rm = TRUE),
    Curtose = e1071::kurtosis(vaa_fundeb, na.rm = TRUE, type = 3),
    Assimetria = e1071::skewness(vaa_fundeb, na.rm = TRUE, type = 3),
    Intervalo = max(vaa_fundeb, na.rm = TRUE) - min(vaa_fundeb, na.rm = TRUE),
    Mínimo = min(vaa_fundeb, na.rm = TRUE),
    Máximo = max(vaa_fundeb, na.rm = TRUE),
    Contagem = sum(!is.na(vaa_fundeb)),
    P10 = quantile(vaa_fundeb, 0.10, na.rm = TRUE),
    P90 = quantile(vaa_fundeb, 0.90, na.rm = TRUE),
    `P90/P10` = quantile(vaa_fundeb, 0.90, na.rm = TRUE) /
      quantile(vaa_fundeb, 0.10, na.rm = TRUE),
    .groups = "drop"
  )

indicadores_tabela <- indicadores_ano %>%
  tidyr::pivot_longer(
    cols = -ano,
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = Valor
  )

boxplot <- base_final %>%
  mutate(
    vaa_fundeb = (rectot_fundeb * defla_ipca) / mat_censo
  ) %>%
  select(cod_ibge, ano, vaa_fundeb) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = vaa_fundeb,
    names_prefix = "vaa_fundeb_"
  )

#2 - Índice de Gini da distribuição dos recursos totais educação municípios de MG, por ano

base_final <- base_final %>% mutate(vaa_total = ((rectot_educ + defla_ipca) / mat_censo))

gini_total_ano <- base_final %>% group_by(ano) %>%  summarize(gini = Gini(vaa_total), .groups = "drop")


#2.5 - Análise estatística do Fundeb entre os municípios de MG, por ano


indicadores_ano_total <- base_final %>%
  group_by(ano) %>%
  summarise(
    Média = mean(vaa_total, na.rm = TRUE),
    Mediana = median(vaa_total, na.rm = TRUE),
    `Desvio padrão` = sd(vaa_total, na.rm = TRUE),
    Curtose = e1071::kurtosis(vaa_total, na.rm = TRUE, type = 3),
    Assimetria = e1071::skewness(vaa_total, na.rm = TRUE, type = 3),
    Intervalo = max(vaa_total, na.rm = TRUE) - min(vaa_total, na.rm = TRUE),
    Mínimo = min(vaa_total, na.rm = TRUE),
    Máximo = max(vaa_total, na.rm = TRUE),
    Contagem = sum(!is.na(vaa_total)),
    P90 = quantile(vaa_total, 0.90, na.rm = TRUE),
    P10 = quantile(vaa_total, 0.10, na.rm = TRUE),
    `P90/P10` = quantile(vaa_total, 0.90, na.rm = TRUE) /
      quantile(vaa_total, 0.10, na.rm = TRUE),
    .groups = "drop"
  )

indicadores_tabela_total <- indicadores_ano_total %>%
  tidyr::pivot_longer(
    cols = -ano,
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  tidyr::pivot_wider(
    names_from = ano,
    values_from = Valor
  )

# Mapas

#Fundeb 2019/2025
dados_2019 <- base_final %>%  filter(ano == 2019) %>%  
  mutate (vaa_fundeb_2019 = ((rectot_fundeb * defla_ipca) / mat_censo)) %>% 
  select (cod_ibge, vaa_fundeb_2019)
  
dados_2025 <- base_final %>%  filter(ano == 2025) %>%  
  mutate (vaa_fundeb_2025 = ((rectot_fundeb * defla_ipca) / mat_censo)) %>% 
  select (cod_ibge, vaa_fundeb_2025)

dados_mapa <- dados_2019 %>% left_join(dados_2025, by = "cod_ibge")

dados_mapa <- dados_mapa %>%  mutate (valor = ((vaa_fundeb_2025 - vaa_fundeb_2019)/ vaa_fundeb_2019) * 100) %>% 
  select (cod_ibge, valor)

#VAA 2025
dados_2025 <- base_final %>%  filter(ano == 2025) %>%  
  mutate (vaa_fundeb_2025 = ((rectot_fundeb * defla_ipca) / mat_censo),  quintil = ntile(vaa_fundeb_2025, 5)) %>% 
  select (cod_ibge, quintil)



## Outliers

indicadores_outliers <- base_final %>%
  mutate(vaa_fundeb = (rectot_fundeb * defla_ipca) / mat_censo) %>%
  group_by(ano) %>%
  mutate(
    Q1 = quantile(vaa_fundeb, .25, na.rm = TRUE),
    Q3 = quantile(vaa_fundeb, .75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lim_inf = Q1 - 1.5 * IQR,
    lim_sup = Q3 + 1.5 * IQR,
    out_sup = vaa_fundeb > lim_sup,
    out_inf = vaa_fundeb < lim_inf,
    outlier = out_sup | out_inf,
    intensidade = case_when(
      out_sup ~ vaa_fundeb / lim_sup - 1,
      out_inf ~ lim_inf / vaa_fundeb - 1,
      TRUE ~ 0
    )
  ) %>%
  summarise(
    `% de outliers` = mean(outlier) * 100,
    `% outliers superiores` = mean(out_sup) * 100,
    `% outliers inferiores` = mean(out_inf) * 100,
    `Intensidade média` = ifelse(any(outlier), mean(intensidade[outlier]), 0),
    `Intensidade máxima` = ifelse(any(outlier), max(intensidade[outlier]), 0),
    .groups = "drop"
  )

base_sem_outliers <- boxplot42 %>%
  group_by(ano) %>%
  mutate(
    Q1 = quantile(vaa_fundeb, .25, na.rm = TRUE),
    Q3 = quantile(vaa_fundeb, .75, na.rm = TRUE),
    IQR = Q3 - Q1,
    lim_inf = Q1 - 1.5 * IQR,
    lim_sup = Q3 + 1.5 * IQR
  ) %>%
  filter(vaa_fundeb >= lim_inf, vaa_fundeb <= lim_sup) %>%
  select(cod_ibge, ano, vaa_fundeb) %>%
  ungroup()

indicadores_sem_outliers <- base_sem_outliers %>%
  group_by(ano) %>%
  summarise(
    Média = mean(vaa_fundeb, na.rm = TRUE),
    Mediana = median(vaa_fundeb, na.rm = TRUE),
    `Desvio padrão` = sd(vaa_fundeb, na.rm = TRUE),
    Curtose = e1071::kurtosis(vaa_fundeb, na.rm = TRUE, type = 3),
    Assimetria = e1071::skewness(vaa_fundeb, na.rm = TRUE, type = 3),
    Intervalo = max(vaa_fundeb, na.rm = TRUE) - min(vaa_fundeb, na.rm = TRUE),
    Mínimo = min(vaa_fundeb, na.rm = TRUE),
    Máximo = max(vaa_fundeb, na.rm = TRUE),
    Contagem = sum(!is.na(vaa_fundeb)),
    P90 = quantile(vaa_fundeb, 0.90, na.rm = TRUE),
    P10 = quantile(vaa_fundeb, 0.10, na.rm = TRUE),
    `P90/P10` = quantile(vaa_fundeb, 0.90, na.rm = TRUE) /
      quantile(vaa_fundeb, 0.10, na.rm = TRUE),
    Gini = ineq::Gini(vaa_fundeb, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -ano,
    names_to = "Indicador",
    values_to = "Valor"
  ) %>%
  pivot_wider(
    names_from = ano,
    values_from = Valor
  )

write.xlsx(
  list(
    dados_mapa = dados_mapa,
    dados_2025 = dados_2025,
    gini_fundeb_ano = gini_fundeb_ano,
    gini_total_ano = gini_total_ano,
    indicadores_tabela = indicadores_tabela,
    indicadores_tabela_total = indicadores_tabela_total,
    boxplot = boxplot
  ),
  file = "data/analises4.2x.xlsx",
  overwrite = TRUE
)


writexl::write_xlsx(base_sem_outliers, "data/base_sem_outliers_codibge.xlsx")
