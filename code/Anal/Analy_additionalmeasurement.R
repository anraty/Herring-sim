library(tidyverse)
library(reshape2)
library(gridExtra)

source("code/functions.R")

#   ladataan simulaatiot, joissa mukana lisämittaukset
res17500 <- readRDS("res/res_addiMeasur_17500kg.rds")
res60000 <- readRDS("res/res_addiMeasur_60000kg.rds")
res120000 <- readRDS("res/res_addiMeasur_120000kg.rds")




#   simulaatioiden siivous
cl17500 <-cl_tbl(res17500)
cl60000 <-cl_tbl(res60000)
cl120000 <-cl_tbl(res120000)
#   sakkotaulut
tbl17 <- res_tbl(cl17500)
tbl60 <- res_tbl(cl60000)
tbl120 <- res_tbl(cl120000)
#   sakon saanti
plot_sakko(tbl17, 17500)
plot_sakko(tbl60, 60000)
plot_sakko(tbl120, 120000)

#   mittausten osuvuus

dens17 <- plot_dens(cl17500, 17500, 8)
map(dens17, plot)

dens60 <- plot_dens(cl60000, 60000, 8)
map(dens60, plot)

dens120 <- plot_dens(cl120000, 120000, 8)
map(dens120, plot)









