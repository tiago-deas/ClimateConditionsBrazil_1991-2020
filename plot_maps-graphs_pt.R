tm_shape(shp = shp_complete) +
  tm_polygons("total",
              title = "Precipitação Mensal (mm)",
              style = "fixed",
              palette = c("sienna1", "khaki", "lightblue", "steelblue1", "blue1", "blue3", "blue4"),
              breaks = c(-Inf, 100, 120, 140, 160, 180, 200, Inf),
              colorNA = "gray",
              textNA = "Sem dados para RO") +
  tm_layout(main.title = "Média da precipitação mensal nos estados brasileiros de 1991 à 2020",
            title.size = 0.5) +
  tm_compass(type = "8star",
             show.labels = 3,
             size = 2,
             position = c(0.75, 0.09)) +
  tm_credits("Fonte: INMET",
             position = 0.78) +
  tmap_options(check.and.fix = T) +
  tm_layout(legend.title.size = 1.1,
            legend.text.size = 0.85) 

----------------------------------------------------------------------------------------------------------
df_complete <- shp_complete@data[2:27, ]

ggplot(data = df_complete, aes(x = reorder(NM_ESTADO, (total)), y = total, fill =  NM_REGIAO)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Média da precipitação mensal nos estados brasileiros de 1991 à 2020",
    subtitle = "Precipitação em milimetros (mm)",
    x = "Estados",
    y = "Média",
    caption = ("Fonte: INMET")) +
  geom_text(label = paste(round(df_complete$total), "mm"), size = 4.5, hjust = 1.1) +
  scale_fill_discrete(name = "Região",
                      labels = c("Centro-Oeste", "Nordeste","Norte", "Sudeste","Sul")) +
  theme(plot.title = element_text(size=24)) +
  theme_classic(base_size = 14) 
-------------------------------------------------------------------------------------------
sc <- sf_stat %>% 
  rename("code" = "Código",
         "Station" = "Nome da Estação")
  
  
    tm_shape(shp = shp_brazil) + 
  tm_borders(alpha = 0.5) +
  tm_shape(shp = sc) + 
  tm_dots(col = "UF",
          size = 0.2) +
  tmap_options(check.and.fix = TRUE)
  

ggplot(station_plot, aes(reorder(UF,(Total)), Total)) +
  geom_col(fill = "navy") +
  coord_flip() +
  labs(
    title = "Estações do INMET por estado",
    subtitle = "Apenas estações do dataset",
    x = "Estado",
    caption = "Fonte :INMET"
  ) +
  theme_classic(base_size = 14)

shp_SC <- shp_brazil 

sc_df <- df_mean %>% 
  filter(UF == "SC") %>% 
  dplyr::select(everything(),-(4:15))

ggplot(sc_df, aes(reorder(sc_df$`Nome da Estação`,(sc_df$annual_mean)), sc_df$annual_mean)) +
  geom_col(fill = "lightblue", binwidth = 0.5) +
  coord_flip() +
  labs(
    title = "Estações do INMET em SC",
    subtitle = "Apenas estações do dataset",
    x = "Cidade",
    y = "Total",
    caption = "Fonte :INMET"
  ) +
  geom_text(label = paste(round(sc_df$annual_mean), "mm"), hjust = 1.3, size = 5) +
  theme_classic(base_size = 18)

--------------------------------------------------------------------------------
  
  ggplot(df_tmin_complete, aes(Lowest ,Altitude)) +
  geom_point(color = "Red2") +
  geom_text(label = paste(df_tmin_complete$Station, df_tmin_complete$UF, sep = " - "), size = 2.3, vjust = -0.7, hjust = 0.4) +
  labs(
    title = "Relação entre Temperatura Mínima e Altitude",
    x = "Temperatura",
    caption = "Fonte: INMET"
  ) +
  scale_y_continuous(labels = paste0(seq(0, 1500, by = 500), "m")) +
  scale_x_continuous(labels = paste0(seq(-10,20, by = 10), "ºC")) +
  theme_classic(base_size = 16)

tm_shape(shp = shp_brazil2) + 
  tm_borders(alpha = 0.5) +
  tm_polygons("Lowest",
              title = "Menor temperatura por estado (ºC)",
              style = "fixed",
              palette = c("blue4", "blue3", "royalblue1", "steelblue1", "lightblue2", "cadetblue1", "khaki","darkorange1"),
              breaks = c(-Inf, -8, -4, 0, 4, 8, 12, 16, Inf),
              colorNA = "gray",
              textNA = "Sem dados de RO e MS") +
  tm_compass(type = "8star",
             show.labels = 3,
             size = 2,
             position = c(0.7, 0.07)) +
  tm_credits("Fonte:INMET",
             position = 0.73) +
  tmap_options(check.and.fix = T) +
  tm_layout(main.title = "Cidade por estado que obteve a menor temperatura entre 1991 e 2020",
            title = "Apenas estações do INMET disponíveis no período",
            title.position = c(0.60, 0.96),
            title.size = 0.8,
            legend.title.size = 1.3,
            legend.text.size = 0.7,
            main.title.size = 1.1) +
  tm_shape(shp = sf_tmin_complete) + 
  tm_dots(col = "red", 
          size = 0.35) +
  tm_text("Station", size = 0.49, col = "black", just = "bottom", ymod = 0.21) 


df_tmin_complete2$Region[df_tmin_complete2$UF %in% c("RS", "SC", "PR")] <- "Sul"
df_tmin_complete2$Region[df_tmin_complete2$UF %in% c("RJ", "SP", "MG", "ES")] <- "Sudeste"
df_tmin_complete2$Region[df_tmin_complete2$UF %in% c("MT", "GO", "DF")] <- "Centro-Oeste"
df_tmin_complete2$Region[df_tmin_complete2$UF %in% c("AM", "RR", "AP", "PA", "TO", "AC")] <- "Norte"
df_tmin_complete2$Region[df_tmin_complete2$UF %in% c("AL", "SE", "BA", "PE", "PB", "CE", "MA", "PI", "RN")] <- "Nordeste"


ggplot(df_tmin_complete2, aes(reorder(UF, (Lowest)), Lowest, fill = Region)) +
  geom_col() +
  geom_text(label = df_tmin_complete2$Lowest, vjust = -0.3, size = 3.7) +
  scale_y_continuous(labels = paste0(seq(-10, 20, by = 10), "ºC")) +
  labs(
    title = "Menor temperatura por estado entre 1991 - 2020",
    subtitle = "Apenas estações do INMET disponíveis nesse período",
    x = "Estado",
    y = "Temperatura",
    caption = "Fonte: INMET",
  ) +
  scale_fill_discrete(name = "Região") +
  theme_classic(base_size = 13)

ggplot(df_tmax_complete, aes(Highest ,Altitude)) +
  geom_point(color = "Red2") +
  geom_text(label = paste(df_tmax_complete$Station, df_tmax_complete$UF, sep = " - "), size = 2.5, vjust = -0.7, hjust = 0.4) +
  labs(
    title = "Relação entre Temperatura Máxima e Altitude",
    x = "Temperatura",
    caption = "Fonte: INMET"
  ) +
  scale_y_continuous(breaks = seq(0, 1200, by = 300), 
                     labels = paste0(seq(0, 1200, by = 300), "m")) +
  scale_x_continuous(breaks = seq(30, 44, by = 4), 
                     labels = paste0(seq(30, 44, by = 4), "ºC")) +
  theme_classic(base_size = 16)

tm_shape(shp = shp_brazil3) + 
  tm_borders(alpha = 0.5) +
  tm_polygons("Highest",
              title = "Maior temperatura por estado (ºC)",
              style = "fixed",
              palette = c("YlOrRd"),
              breaks = c(-Inf, 36, 38, 40, 42, Inf),
              colorNA = "gray",
              textNA = "Sem dados de RO") +
  tm_layout(main.title = "Cidade por estado que obteve a maior temperatura entre 1991 e 2020",
            title = "Apenas estações do INMET disponíveis no período",
            title.position = c(0.60, 0.96),
            title.size = 0.8,) + 
  tm_compass(type = "8star",
             show.labels = 3,
             size = 2,
             position = c(0.7, 0.07)) +
  tm_credits("Fonte :INMET",
             position = 0.73) +
  tmap_options(check.and.fix = T) +
  tm_layout(legend.title.size = 1.2,
            legend.text.size = 0.8,
            main.title.size = 1) +
  tm_shape(shp = sf_tmax_complete) + 
  tm_dots(col = "blue", 
          size = 0.4) +
  tm_text("Station", size = 0.49, col = "black", just = "bottom", ymod = 0.21) 

df_tmax_complete2$Region[df_tmax_complete2$UF %in% c("RS", "SC", "PR")] <- "Sul"
df_tmax_complete2$Region[df_tmax_complete2$UF %in% c("RJ", "SP", "MG", "ES")] <- "Sudeste"
df_tmax_complete2$Region[df_tmax_complete2$UF %in% c("MT", "GO", "DF", "MS")] <- "Centro-Oeste"
df_tmax_complete2$Region[df_tmax_complete2$UF %in% c("AM", "RR", "AP", "PA", "TO", "AC")] <- "Norte"
df_tmax_complete2$Region[df_tmax_complete2$UF %in% c("AL", "SE", "BA", "PE", "PB", "CE", "MA", "PI", "RN")] <- "Nordeste"

ggplot(df_tmax_complete2, aes(reorder(UF, (Highest)), Highest, fill = Region)) +
  geom_col() +
  geom_text(label = paste0(df_tmax_complete2$Highest, "ºC"), hjust = 1.2, size = 3.5) +
  coord_flip() +
  labs(title = "Maior temperatura por estado entre 1991 - 2020",
       subtitle = "Apenas estações do INMET disponíveis nesse período",
       x = "Estado",
       y = "Temperatura",
       caption = "Fonte: INMET") +
  theme_classic(base_size = 13) +
  scale_y_continuous(breaks = seq(30, 44, by = 4), 
                     labels = paste0(seq(30, 44, by = 4), "ºC")) +
  scale_fill_discrete(name = "Região")

----------------------------------------------
  
  tmap_mode("view")

tm_shape(shp = sf_tmin) + 
  tm_dots(col = "UF", 
          border.col = "black", 
          size = 0.1, 
          alpha = 0.8)

tm_shape(shp = sf_tmax) + 
  tm_dots(col = "UF", 
          border.col = "black", 
          size = 0.1, 
          alpha = 0.8)
