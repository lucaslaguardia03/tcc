library(dplyr)
library(data.table)
library(readxl)
library(readODS)

base_final <- readxl::read_excel("data/base_final.xlsx")
ipea <- read_ods("data_raw/ipea_ate_2024.ods") 


# Arrecadação anual cesta Fundeb

arrecad_fundeb_mun <- base_final %>% 
  select(impadd_fundeb, ano, defla_ipca) %>% 
  mutate(
    cesta_fundeb_total = impadd_fundeb * 20
  ) %>% 
  group_by(ano) %>% 
  summarize(
    cesta_fundeb_total = sum(cesta_fundeb_total, na.rm = TRUE),
    defla_ipca = first(defla_ipca)
  ) %>% 
  mutate(
    cesta_fundeb_total_mun = cesta_fundeb_total * defla_ipca
  ) %>%  select (cesta_fundeb_total_mun, ano)

arrecad_fundeb_estado_ate24 <- ipea %>% filter(ente == "Estadual", sig_uf == "MG",ano>= 2018) %>% 
  select(impadd_fundeb, ano, defla_ipca) %>% 
  mutate(
    cesta_fundeb_total = impadd_fundeb * 20
  ) %>% 
  group_by(ano) %>% 
  summarize(
    cesta_fundeb_total = sum(cesta_fundeb_total, na.rm = TRUE),
    defla_ipca = first(defla_ipca)
  ) %>% 
  mutate(
    cesta_fundeb_total_MG_ate24 = cesta_fundeb_total * defla_ipca
  )  %>%  select (cesta_fundeb_total_MG_ate24, ano)

#Preenchido manualmente, tendo em vista que a consulta é de um ano só e a base tem 14GB e demora pra carregar. Fonte:https://www.fazenda.mg.gov.br/export/sites/fazenda/governo/contadoria_geral/lrf/2025/6-bimestre/anexo8.pdf

arrecad_fundeb_estado_2025 <- data.frame(
  arrecad_fundeb_estado_2025 = 17169634074.70 * 5,
  ano = 2025
)

arrecad_fundeb <- arrecad_fundeb_mun %>% 
  left_join(arrecad_fundeb_estado_ate24, by = "ano") %>% 
  left_join(arrecad_fundeb_estado_2025, by = "ano")

library(openxlsx)

write.xlsx(
  arrecad_fundeb,
  "data/arrecad_fundeb.xlsx",
  rowNames = FALSE
)