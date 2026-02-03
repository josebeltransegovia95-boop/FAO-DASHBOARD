
rm(list=ls())
gc()

#unlink("C:/Users/jose2/AppData/Local/R/win-library/4.5/00LOCK", recursive = TRUE)

pacman::p_load(rvest, dplyr, tidytext, tm, tidyverse,
               ggplot2, wordcloud, wordcloud2, plotly,
               igraph, ggraph, tidygraph, forcats, httr,
               KoboconnectR, devtools, haven, readxl,
               stringi, readxl, labelled, units, sf,
               sp, spdep, leaflet, tidyverse,
               scatterplot3d, ggrepel, cowplot, ggmap,
               writexl, plotly, tidyr, rsconnect, DT,
               bs4Dash, fresh, openxlsx)

setwd("/Users/josedavidbeltransegovia/OneDrive/R/FAO-DASHBOARD/EMPLEO_JOVEN")

load("base_datos.Rdata")
load("base_datos_desc.Rdata")
load("geo_prov.Rdata")

names(df_final)

df_final$nom_empr<- NULL

names(df_final)

save(df_final, file = "base_datos.Rdata")

