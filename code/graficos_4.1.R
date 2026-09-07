##Gráficos 4.1 Panorama do financiamento da educação básica após o Novo Fundeb

library(vroom)
library(dplyr)
library(data.table)
library(readxl)

base_final <- readxl::read_excel("data/base_final.xlsx")

# Receita Total Fundeb Municípios por ano - Gráfico 1
rectot_fundeb_mun <- base_final %>% 
  filter (ente == "Municipal", cod_uf == 31, ano >= 2018) %>% 
  mutate(rectot_fundeb_defl = rectot_fundeb * defla_ipca) %>% 
  group_by(ano) %>%
  summarise(
    rectot_fundeb_defl = sum(rectot_fundeb_defl, na.rm = TRUE),
    mat_censo = sum(mat_censo, na.rm = TRUE),
    rectot_por_ma = rectot_fundeb_defl / mat_censo,
    .groups = "drop"
  ) %>% select(ano, rectot_por_ma)


# Recursos Totais Educação  - Gráfico 2
rectot_edu <- base_final %>% 
  filter(ente == "Municipal",cod_uf == 31,ano >= 2018) %>% 
  mutate(
    rectot_fundeb_defl = rectot_fundeb * defla_ipca,
    royalty_educ_defl = royalty_educ * defla_ipca,
    impadd_mde_defl = impadd_mde * defla_ipca,
    impadd_fundeb_defl = impadd_fundeb * defla_ipca,
    sal_educ_defl = sal_educ * defla_ipca,
    prog_fnde_defl = prog_fnde * defla_ipca,
    outrec_educ_defl = outrec_educ * defla_ipca
  ) %>% 
  group_by(ano) %>%
  summarise(
    rectot_fundeb_defl = sum(rectot_fundeb_defl, na.rm = TRUE),
    royalty_educ_defl = sum(royalty_educ_defl, na.rm = TRUE),
    impadd_mde_defl = sum(impadd_mde_defl, na.rm = TRUE),
    impadd_fundeb_defl = sum(impadd_fundeb_defl, na.rm = TRUE),
    sal_educ_defl = sum(sal_educ_defl, na.rm = TRUE),
    prog_fnde_defl = sum(prog_fnde_defl, na.rm = TRUE),
    outrec_educ_defl = sum(outrec_educ_defl, na.rm = TRUE),
    mat_censo = sum(mat_censo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    rectot_fundeb_defl = rectot_fundeb_defl / mat_censo,
    royalty_educ_defl = royalty_educ_defl / mat_censo,
    impadd_mde_defl = impadd_mde_defl / mat_censo,
    impadd_fundeb_defl = impadd_fundeb_defl / mat_censo,
    sal_educ_defl = sal_educ_defl / mat_censo,
    prog_fnde_defl = prog_fnde_defl / mat_censo,
    outrec_educ_defl = outrec_educ_defl / mat_censo,
  ) %>%
  select(
    ano,
    rectot_fundeb_defl,
    royalty_educ_defl,
    impadd_mde_defl,
    impadd_fundeb_defl,
    sal_educ_defl,
    prog_fnde_defl,
    outrec_educ_defl,
  )

# PERCENTUAL Recursos Totais Educação  - Tabela
perc_rectot_edu <- rectot_edu %>%
  mutate(
    total_educ = rectot_fundeb_defl +
      royalty_educ_defl +
      impadd_mde_defl +
      impadd_fundeb_defl +
      sal_educ_defl +
      prog_fnde_defl +
      outrec_educ_defl
  ) %>%
  mutate(
    perc_rectot_fundeb = rectot_fundeb_defl / total_educ,
    perc_royalty_educ = royalty_educ_defl / total_educ,
    perc_impadd_mde = impadd_mde_defl / total_educ,
    perc_impadd_fundeb = impadd_fundeb_defl / total_educ,
    perc_sal_educ = sal_educ_defl / total_educ,
    perc_prog_fnde = prog_fnde_defl / total_educ,
    perc_outrec_educ = outrec_educ_defl / total_educ
  ) %>%
  select(
    ano,
    perc_rectot_fundeb,
    perc_royalty_educ,
    perc_impadd_mde,
    perc_impadd_fundeb,
    perc_sal_educ,
    perc_prog_fnde,
    perc_outrec_educ
  )


# Participação dos Municípios no Fundo - Gráfico 3

stn_transferencias_MG <- fread("data_raw/stn_transferencia_MG.csv", sep = ";", encoding = "Latin-1") 

ipca <- read_excel("data_raw/ipca.xlsx") %>%
  select(ano, defla_ipca)

stn_transferencias_MG <- stn_transferencias_MG %>%
  filter(ano %in% 2018:2025) %>% 
  left_join(ipca, by = "ano")

rectot_fundeb_mun <- base_final %>% 
  filter (ente == "Municipal", cod_uf == 31, ano >= 2018) %>% 
  mutate(rectot_fundeb_defl = rectot_fundeb * defla_ipca) %>% 
  group_by(ano) %>%
  summarise(
    rectot_fundeb_defl = sum(rectot_fundeb_defl, na.rm = TRUE),
    mat_censo = sum(mat_censo, na.rm = TRUE),
    rectot_por_ma = rectot_fundeb_defl / mat_censo,
    .groups = "drop"
  )

rectot_fundeb_MG <- stn_transferencias_MG  %>% 
  mutate(VL_CONSOLIDADO = VL_CONSOLIDADO * defla_ipca) %>% 
  group_by(ano) %>%
  summarise(
    rectot_fundeb_MG_defl = sum(VL_CONSOLIDADO, na.rm = TRUE),
    .groups = "drop"
  )

comparacao_participacao <- rectot_fundeb_MG %>% 
  left_join(rectot_fundeb_mun, by = "ano") %>% 
  select(ano, rectot_fundeb_defl, rectot_fundeb_MG_defl) %>% 
  mutate(partici_mun = rectot_fundeb_defl / (rectot_fundeb_defl + rectot_fundeb_MG_defl))


# Composição do FUNDEB - Gráfico 4
composicao_fundeb <- base_final %>% 
  filter(ente == "Municipal",cod_uf == 31,ano >= 2018) %>% 
  mutate(
    transf_fundeb_defl = transf_fundeb * defla_ipca,
    compl_vaaf_defl = compl_vaaf * defla_ipca,
    compl_vaat_defl = compl_vaat * defla_ipca,
    compl_vaar_defl = compl_vaar * defla_ipca
  ) %>% 
  group_by(ano) %>%
  summarise(
    transf_fundeb_defl = sum(transf_fundeb_defl, na.rm = TRUE),
    compl_vaaf_defl = sum(compl_vaaf_defl, na.rm = TRUE),
    compl_vaat_defl = sum(compl_vaat_defl, na.rm = TRUE),
    compl_vaar_defl = sum(compl_vaar_defl, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(
    perc_transf_fundeb = transf_fundeb_defl / 
      (transf_fundeb_defl + compl_vaaf_defl + compl_vaat_defl + compl_vaar_defl),
    
    perc_compl_vaaf = compl_vaaf_defl / 
      (transf_fundeb_defl + compl_vaaf_defl + compl_vaat_defl + compl_vaar_defl),
    
    perc_compl_vaat = compl_vaat_defl / 
      (transf_fundeb_defl + compl_vaaf_defl + compl_vaat_defl + compl_vaar_defl),
    
    perc_compl_vaar = compl_vaar_defl / 
      (transf_fundeb_defl + compl_vaaf_defl + compl_vaat_defl + compl_vaar_defl)
  )

library(openxlsx)

write.xlsx(
  list(
    rectot_fundeb_mun = rectot_fundeb_mun,
    rectot_edu        = rectot_edu,
    perc_rectot_edu   = perc_rectot_edu,
    rectot_fundeb_MG  = rectot_fundeb_MG,
    composicao_fundeb = composicao_fundeb,
    comparacao_participacao = comparacao_participacao
  ),
  file = "data/graficos4.11.xlsx",
  overwrite = TRUE
)