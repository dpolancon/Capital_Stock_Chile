############################################################
## 10_auditoria_sfc_reconstruction.R
## Auditoría integral del script 10_sfc_reconstruction.R
##
## Objetivo:
##   - Evaluar calidad de la reconstrucción SFC 1900–1994
##   - Detectar años problemáticos, rupturas, inconsistencias
##   - Verificar identities forward / backward
##   - Comparar Kg, Kn reconstruidos vs observados (1950–1994)
##   - Generar métricas para ajuste posterior (script 11)
##
## Salidas:
##   - outputs/auditoria_10/*.png
##   - outputs/auditoria_10/summary_tables.rds
##
############################################################

library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(purrr)
library(stringr)
library(here)

## ---------------------------------------------------------
## 0. Paths & Load Data
## ---------------------------------------------------------
dir_proc <- here("data","processed")
dir_int  <- here("data","interim")
dir_out  <- here("outputs","auditoria_10")

if(!dir.exists(dir_out)) dir.create(dir_out, recursive = TRUE)

panel_recon <- readRDS(file.path(dir_proc, "panel_recon_2003.rds"))
panel_orig  <- readRDS(file.path(dir_int,  "panel_sfc_2003.rds"))

year_min <- 1900
year_max <- 1994
year_anchor <- 1950

assets <- sort(unique(panel_recon$asset))

cat("\n============================================\n")
cat("   AUDITORÍA — RECONSTRUCCIÓN SFC (script 10)\n")
cat("   Ventana analizada:", year_min, "–", year_max, "\n")
cat("============================================\n\n")

## ---------------------------------------------------------
## Helper: Save plot
## ---------------------------------------------------------
save_plot <- function(g, name, w=8, h=5){
  ggsave(filename = file.path(dir_out, paste0(name,".png")),
         plot = g, width=w, height=h, dpi=300)
}

## ---------------------------------------------------------
## 1. Cobertura: ¿qué reconstruimos realmente?
## ---------------------------------------------------------

cat(">>> Resumen de cobertura por activo:\n")
cover <- panel_recon %>%
  mutate(valid_Kg = !is.na(Kg_recon),
         valid_Kn = !is.na(Kn_recon)) %>%
  group_by(asset) %>%
  summarise(
    n_total = n(),
    n_valid_Kg = sum(valid_Kg),
    n_valid_Kn = sum(valid_Kn),
    pct_Kg = 100*n_valid_Kg/n_total,
    pct_Kn = 100*n_valid_Kn/n_total
  )
print(cover)
cat("\n")

## ---------------------------------------------------------
## 2. Auditoría identidad forward:
##   Kg_recon(t+1) ?= Kg_recon(t) + Ig(t+1) - R(t+1)
## ---------------------------------------------------------

aud_forward <- panel_recon %>%
  arrange(asset, year) %>%
  group_by(asset) %>%
  mutate(
    Kg_fwd_calc = lag(Kg_recon) +
      Ig - (lag(Kg_recon) * (z_clean/1000)),
    Kn_fwd_calc = lag(Kn_recon) +
      Ig - (lag(Kn_recon) * (delta_clean/1000)),
    err_forward_Kg = Kg_recon - Kg_fwd_calc,
    err_forward_Kn = Kn_recon - Kn_fwd_calc
  ) %>% ungroup()

forward_summary <- aud_forward %>%
  filter(year > year_anchor) %>%
  group_by(asset) %>%
  summarise(
    mean_err_Kg = mean(err_forward_Kg, na.rm=TRUE),
    sd_err_Kg   = sd(err_forward_Kg, na.rm=TRUE),
    mean_err_Kn = mean(err_forward_Kn, na.rm=TRUE),
    sd_err_Kn   = sd(err_forward_Kn, na.rm=TRUE)
  )

cat(">>> Identidad forward — errores resumen:\n")
print(forward_summary)
cat("\n")

## ---------------------------------------------------------
## 3. Auditoría backward:
##   Kg_recon(t−1) ?= (Kg_recon(t) − Ig(t)) / (1 − z/1000)
## ---------------------------------------------------------

aud_backward <- panel_recon %>%
  arrange(asset, year) %>%
  group_by(asset) %>%
  mutate(
    fac_g = (1 - z_clean/1000),
    fac_n = (1 - delta_clean/1000),
    Kg_back_calc = (lead(Kg_recon) - lead(Ig)) / lead(fac_g),
    Kn_back_calc = (lead(Kn_recon) - lead(Ig)) / lead(fac_n),
    err_back_Kg = Kg_recon - Kg_back_calc,
    err_back_Kn = Kn_recon - Kn_back_calc
  ) %>% ungroup()

back_summary <- aud_backward %>%
  filter(year < year_anchor) %>%
  group_by(asset) %>%
  summarise(
    mean_err_Kg = mean(err_back_Kg, na.rm=TRUE),
    sd_err_Kg   = sd(err_back_Kg, na.rm=TRUE),
    mean_err_Kn = mean(err_back_Kn, na.rm=TRUE),
    sd_err_Kn   = sd(err_back_Kn, na.rm=TRUE)
  )

cat(">>> Identidad backward — errores resumen:\n")
print(back_summary)
cat("\n")

## ---------------------------------------------------------
## 4. Comparación vs observados (1950–1994)
## ---------------------------------------------------------

comp <- panel_recon %>%
  filter(year >= year_anchor, year <= year_max) %>%
  group_by(asset) %>%
  summarise(
    mean_rel_Kg = mean(rel_error_Kg, na.rm=TRUE),
    sd_rel_Kg   = sd(rel_error_Kg,   na.rm=TRUE),
    mean_rel_Kn = mean(rel_error_Kn, na.rm=TRUE),
    sd_rel_Kn   = sd(rel_error_Kn,   na.rm=TRUE)
  )

cat(">>> Comparación reconstruido vs observado (1950–1994):\n")
print(comp)
cat("\n")

## ---------------------------------------------------------
## 5. Plots por activo
## ---------------------------------------------------------

for(a in assets){
  
  df <- panel_recon %>% filter(asset == a)
  
  ## --- Kg recon vs observado
  g1 <- ggplot(df, aes(year)) +
    geom_line(aes(y=Kg, color="Observed"), size=1) +
    geom_line(aes(y=Kg_recon, color="Reconstructed"), size=1) +
    labs(title=paste("Kg observado vs reconstruido —", a),
         y = "Kg") +
    scale_color_manual(values=c("Observed"="black","Reconstructed"="red")) +
    theme_minimal()
  save_plot(g1, paste0("Kg_obs_vs_rec_", a))
  
  ## --- Kn recon vs observado
  g2 <- ggplot(df, aes(year)) +
    geom_line(aes(y=Kn, color="Observed"), size=1) +
    geom_line(aes(y=Kn_recon, color="Reconstructed"), size=1) +
    labs(title=paste("Kn observado vs reconstruido —", a),
         y = "Kn") +
    scale_color_manual(values=c("Observed"="black","Reconstructed"="blue")) +
    theme_minimal()
  save_plot(g2, paste0("Kn_obs_vs_rec_", a))
  
  ## --- Errores relativos
  g3 <- ggplot(df, aes(year, rel_error_Kg)) +
    geom_hline(yintercept=0, color="grey50") +
    geom_line(color="red") +
    labs(title=paste("Error relativo Kg —", a),
         y="(Kg_rec − Kg)/Kg") +
    theme_minimal()
  save_plot(g3, paste0("error_rel_Kg_", a))
  
  g4 <- ggplot(df, aes(year, rel_error_Kn)) +
    geom_hline(yintercept=0, color="grey50") +
    geom_line(color="blue") +
    labs(title=paste("Error relativo Kn —", a),
         y="(Kn_rec − Kn)/Kn") +
    theme_minimal()
  save_plot(g4, paste0("error_rel_Kn_", a))
  
  ## --- Chequeo del factor de estabilidad (1−δ), (1−z)
  g5 <- ggplot(df, aes(year)) +
    geom_line(aes(y = 1 - delta_clean/1000, color="1 - δ"), size=1) +
    geom_line(aes(y = 1 - z_clean/1000,     color="1 - z"), size=1) +
    labs(title=paste("Factores de estabilidad —", a),
         y="Factor") +
    scale_color_manual(values=c("1 - δ"="red","1 - z"="blue")) +
    theme_minimal()
  save_plot(g5, paste0("stability_factors_", a))
}

## ---------------------------------------------------------
## 6. Export summary tables
## ---------------------------------------------------------

summary_list <- list(
  coverage = cover,
  forward_errors = forward_summary,
  backward_errors = back_summary,
  comparison = comp
)

saveRDS(summary_list, file.path(dir_out, "summary_tables.rds"))

cat("\n============================================\n")
cat("Auditoría completada. Salidas en outputs/auditoria_10/\n")
cat("============================================\n\n")
