suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})

arq <- c("data/resultados_tcc_fundeb.xlsx", "../data/resultados_tcc_fundeb.xlsx")
arq <- arq[file.exists(arq)][1]
if (is.na(arq)) arq <- file.choose()

raw <- read_excel(arq, sheet = "GF_Base_Municipios", .name_repair = "minimal")

d <- tibble(
  var = as.numeric(raw[["variacao_recfundeb_18-25"]]),
  rec = as.numeric(raw$rec_educ_por_mat) / 1000,
  cad = as.numeric(raw$pct_pop_cadunico),
  rural = as.numeric(raw$pct_mat_rural),
  creche = as.numeric(raw$pct_mat_creche),
  especial = as.numeric(raw$pct_mat_especial),
  qi = as.numeric(raw$pct_mat_quilombola_indigena),
  eja = as.numeric(raw$pct_mat_eja),
  integral = as.numeric(raw$pct_mat_integral)
) %>% filter(complete.cases(.))

d$ylog <- log(1 + d$var / 100)

nos <- c(6.000, 6.250, 6.500, 7.000)

bic <- sapply(nos, function(k) BIC(lm(ylog ~ rec + pmax(rec - k, 0) + cad, data = d)))

resultado <- data.frame(
  no_R_mil = nos,
  BIC = round(bic, 2),
  dif_BIC_vs_6.5 = round(bic - bic[nos == 6.5], 1)
)

cat("Municípios:", nrow(d), "\n\n")
print(resultado, row.names = FALSE)
