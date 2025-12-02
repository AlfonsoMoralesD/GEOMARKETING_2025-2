# --- CARGA DE LIBRERIAS ---
library(haven)
library(pROC)
library(mgcv)
library(ggplot2)

# --- CARGA DE DATOS EPF ---
personas    <- read_dta("Data/datos_epf/EPF/base-personas-ix-epf-stata.dta") 
gastos      <- read_dta("Data/datos_epf/EPF/base-gastos-ix-epf-stata.dta") 
cantidades  <- read_dta("Data/datos_epf/EPF/base-cantidades-ix-epf-stata.dta") 
ccif        <- read_dta("Data/datos_epf/EPF/ccif-ix-epf-stata.dta") 

