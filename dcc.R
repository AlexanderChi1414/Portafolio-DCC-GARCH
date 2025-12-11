# --- 1. Importación de librerías --- ####

library(tidyquant)
library(tidyverse)
library(rmgarch)
library(PortfolioAnalytics)
library(PerformanceAnalytics)
library(ROI)
library(ROI.plugin.quadprog)
library(ROI.plugin.glpk)
library(DEoptim)

# --- 2. Carga de insumos --- ####

# Definir tickers
tickers = c('WMT', 'JNJ', 'NKE', 'CROX')

# Descargar datos
getSymbols(tickers, from = '2023-01-01', to = Sys.Date())

# Combinar las variables
df <- cbind(WMT$WMT.Close, JNJ$JNJ.Close, NKE$NKE.Close, CROX$CROX.Close)

# Calcular rendimientos logaritmicos
dfr <- na.omit(diff(log(df)))

# --- 3. DCC GARCH --- ####

# GARCH
uspec <- ugarchspec(
  mean.model = list(armaOrder = c(0, 0)),
  variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
  distribution.model = "std"
)

# DCC
spec_dcc <- dccspec(
  uspec = multispec(replicate(ncol(dfr), uspec)),  
  dccOrder = c(1, 1),
  distribution = "mvt"
)

# DCC GARCH para la dataset
fit_dcc <- dccfit(spec_dcc, data = dfr)

# Ver resultado
fit_dcc

# Matriz de correlación
cor_dcc <- rcor(fit_dcc)

# Matriz de covarianza
cov_dcc <- rcov(fit_dcc)

# Exportar los arrays
saveRDS(cor_dcc, "C:/Users/Alexander/OneDrive/桌面/Proyecto final mercado de capitales/cor_dcc.rds")
saveRDS(cov_dcc, "C:/Users/Alexander/OneDrive/桌面/Proyecto final mercado de capitales/cov_dcc.rds")
