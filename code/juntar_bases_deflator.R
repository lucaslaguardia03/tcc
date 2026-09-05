
library(data.table)
library(dplyr)
library(readODS)
library(readxl)
library(openxlsx)


cols_numericas <- c(
  "rectot_fundeb",
  "transf_fundeb",
  "compl_uni",
  "compl_vaaf",
  "compl_vaat",
  "compl_vaar",
  "royalty_educ",
  "contrib_fundeb",
  "impadd_fundeb",
  "impadd_mde",
  "sal_educ",
  "prog_fnde",
  "outrec_educ",
  "recadd_educ",
  "rectot_educ",
  "mat_censo",
  "defla_ipca"
)


#juntar bases
base_2025 <- fread("data/base_2025.csv", sep = ";")

base_2025[, (cols_numericas) := lapply(.SD, \(x)
                                       as.numeric(gsub(",", ".", gsub(".", "", x, fixed = TRUE)))
), .SDcols = cols_numericas]

ipea <- read_ods("data_raw/ipea_ate_2024.ods") %>% filter (ente == "Municipal", cod_uf == 31, ano >= 2018)

# Importa o IPCA
ipca <- read_excel("data_raw/ipca.xlsx") %>%
  select(ano, defla_ipca)

base_final <- rbindlist(list(ipea, base_2025), use.names = TRUE)

# Substitui o defla_ipca de base_final conforme o ano
base_final <- base_final %>%
  select(-defla_ipca) %>%
  left_join(ipca, by = "ano")



base_final[, (cols_numericas) := lapply(.SD, as.numeric),
           .SDcols = cols_numericas]

write.xlsx(
  base_final,
  "data/base_final.xlsx",
  overwrite = TRUE
)
