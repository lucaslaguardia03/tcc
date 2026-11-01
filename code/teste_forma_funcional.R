pkgs <- c("readxl", "openxlsx", "sandwich", "lmtest")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
suppressPackageStartupMessages({
  library(readxl); library(openxlsx); library(sandwich); library(lmtest)
})

arq_entrada <- "data_raw/base_correlacoes.xlsx"
arq_saida   <- "data/teste_forma_funcional.xlsx"
dir.create("data", showWarnings = FALSE)

NOS         <- seq(5, 9, by = 0.25)
SORTEIOS    <- 300
PERMUTACOES <- 1000
SEMENTE     <- 20261101

raw <- read_excel(arq_entrada, sheet = "Planilha2", .name_repair = "minimal")
d <- data.frame(
  y   = log(1 + as.numeric(raw[["variacao_recfundeb_18-25"]]) / 100) * 100,
  rec = as.numeric(raw$rec_educ_por_mat) / 1000,
  rcl = log(as.numeric(raw$receita_corrente_liquida_pc) / 1000),
  cad = as.numeric(raw$pct_pop_cadunico)
)
d <- d[complete.cases(d), ]

ajusta <- function(x, k) lm(y ~ pmin(rec, k) + pmax(rec - k, 0) + rcl + cad, data = x)
escolhe_no <- function(x) NOS[which.min(sapply(NOS, function(k) BIC(ajusta(x, k))))]
t_abaixo <- function(x, k) {
  m <- ajusta(x, k)
  coeftest(m, vcov. = vcovHC(m, type = "HC3"))[2, 3]
}
t_max <- function(x) max(sapply(NOS, function(k) abs(t_abaixo(x, k))))

n <- nrow(d)

set.seed(SEMENTE)
divisao <- do.call(rbind, lapply(seq_len(SORTEIOS), function(i) {
  idx <- sample(n, floor(n / 2))
  a <- d[idx, ]
  b <- d[-idx, ]
  k <- escolhe_no(a)
  m <- suppressWarnings(ajusta(b, k))
  ct <- suppressWarnings(coeftest(m, vcov. = vcovHC(m, type = "HC3")))
  data.frame(sorteio = i, no_escolhido = k,
             coef_abaixo = ct[2, 1], p_abaixo = ct[2, 4],
             coef_acima = ct[3, 1], p_acima = ct[3, 4])
}))

divisao_resumo <- data.frame(
  medida = c("sorteios", "no_mediano", "no_quartil_25", "no_quartil_75",
             "coef_abaixo_mediano", "coef_abaixo_quartil_25", "coef_abaixo_quartil_75",
             "prop_abaixo_significativo_5pct", "prop_acima_significativo_5pct"),
  valor = c(SORTEIOS, median(divisao$no_escolhido),
            quantile(divisao$no_escolhido, 0.25), quantile(divisao$no_escolhido, 0.75),
            median(divisao$coef_abaixo),
            quantile(divisao$coef_abaixo, 0.25), quantile(divisao$coef_abaixo, 0.75),
            mean(divisao$p_abaixo < 0.05), mean(divisao$p_acima < 0.05)),
  row.names = NULL
)

obs <- suppressWarnings(t_max(d))
m0 <- lm(y ~ rcl + cad, data = d)
ajustado <- fitted(m0)
residuo <- resid(m0)

set.seed(SEMENTE + 1)
perm <- vapply(seq_len(PERMUTACOES), function(i) {
  dd <- d
  dd$y <- ajustado + sample(residuo)
  suppressWarnings(t_max(dd))
}, numeric(1))

busca_resumo <- data.frame(
  medida = c("permutacoes", "t_max_observado", "t_max_critico_95pct",
             "p_ingenuo_segmento_abaixo", "p_corrigido_pela_busca"),
  valor = c(PERMUTACOES, obs, quantile(perm, 0.95),
            { m <- ajusta(d, 6.5); coeftest(m, vcov. = vcovHC(m, type = "HC3"))[2, 4] },
            (1 + sum(perm >= obs)) / (PERMUTACOES + 1)),
  row.names = NULL
)
busca_distribuicao <- data.frame(permutacao = seq_along(perm), t_max = perm)

wb <- createWorkbook()
add <- function(nome, x) { addWorksheet(wb, nome); writeData(wb, nome, x) }
add("divisao_amostra", divisao)
add("divisao_resumo", divisao_resumo)
add("busca_resumo", busca_resumo)
add("busca_distribuicao", busca_distribuicao)
saveWorkbook(wb, arq_saida, overwrite = TRUE)
