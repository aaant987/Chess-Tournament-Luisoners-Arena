
#CAMBIAR AQUAMARINE POR FILL SCALEFILLGRADIENT

library(tidyverse)

library(bigchess)

library(patchwork)
library(ragg)

library(extrafont)
windowsFonts("Bahnschrift" = windowsFont("Bahnschrift"))

library(ggrepel)


df1 <- read.csv("lichess_tournament_2021.04.30_UJyeH8oF_i-torneo-luisoners.csv")
df2 <- read.csv("lichess_tournament_2021.05.14_oyN6nfa4_ii-torneo-luisoners.csv")
df3 <- read.csv("lichess_tournament_2021.05.28_jBpvbJzr_iii-torneo-luisoners.csv")
df4 <- read.csv("lichess_swiss_2021.06.18_RooCpAzR_iv-torneo-luisoners.csv")
df5 <- read_csv("lichess_tournament_2021.07.16_NGYZktjj_v-torneo-luisoners (1).csv")
df6 <- read.csv("lichess_tournament_2021.08.27_NxCUTCny_vi-torneo-luisoners.csv")

df1 <- df1 %>% 
  mutate(id = 1)

df2 <- df2 %>% 
  mutate(id = 2)

df3 <- df3 %>% 
  mutate(id = 3)

df4 <- df4 %>% 
  mutate(id = 4)

df5 <- df5 %>% 
  mutate(id = 5)

df6 <- df6 %>% 
  mutate(id = 6)

df <- bind_rows(df1, df2, df3, df4, df5, df6)
df$id <- as.factor(df$id)
df$Username <- as.factor(df$Username)

# los 17 que han participado en todas las ediciones ----------------

df %>% 
  count(Username) %>% 
  arrange(desc(n)) %>% 
  head(n = 17) 


# tabla de honor  -----------------

my_theme <- theme(text =element_text(family = "Bahnschrift", face = "bold"),
                  plot.background = element_rect(fill = "gray95"),
                  panel.background = element_rect(fill = "gray95"),
                  panel.grid = element_blank(),
                  panel.spacing = unit(2.5, units = "cm"),
                  plot.title = element_text(family = "Bahnschrift", size = 16, hjust = 0.5, color = "black"),
                  plot.subtitle = element_text(family = "Bahnschrift", size = 12),
                  plot.caption = element_text(color = "black", size = 10),
                  legend.position = "top", 
                  axis.text.x = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black"),
                  axis.text.y = element_text(vjust = 0.5, hjust=0.5, size = 9, color = "black")) 

p1 <- df %>% 
  group_by(Username, id, Rank) %>% 
  count(wt = Score, sort = T) %>% 
  head(n = 60) %>% 
  ggplot(aes(x = as.factor(id),  y = reorder(Username, -Rank))) +
  geom_tile(aes(fill = n), size = 0.75, color = "#F0F0F0") +
  geom_text(aes(label=Rank), family = "Bahnschrift", color = "blue", size = 3.6) +
  scale_x_discrete(breaks = seq(1, 6, 1), labels=c("1" = "I", 
                                                   "2" = "II",
                                                   "3" = "III",
                                                   "4" = "IV",
                                                   "5" = "V",
                                                   "6" = "VI"),
                   name = "Edición") +
  scale_y_discrete(name = "") +
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold"), name = "Score") +
  labs(
    title = "Posiciones de honor Luisoners Arena",
    subtitle = "*La IV edición fue en sistema suizo",
    caption = ""
  ) +
  my_theme +
  theme(legend.background = element_rect(fill="gray95", 
                                   size=0.5, linetype="solid"))

# quién ha sumado más score ------------------

p2 <- df %>% 
  group_by(Username) %>% 
  count(Username, wt = Score, sort = T) %>% 
  head(n = 10) %>% 
  ungroup() %>% 
  ggplot(aes(n, reorder(Username, -n),
             fill = n)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = n), vjust = 1, family = "Bahnschrift", color = "blue", size = 3.6) +
  labs(
    title = "Top 10 histórico jugadores con más Score",
    subtitle = "",
    caption = "",
    x = "Score", 
    y = ""
  ) +
  my_theme +
  theme(axis.text.x = element_text(size = 7)) +
    theme(legend.position = "none") +
  coord_flip()


  
# ---- evolucion inscritos por edicion y rating medio ------------

evol <- df %>% 
         count(id) 

evol1 <- df %>% 
  group_by(id) %>% 
  summarise(mean_rating = mean(round(Rating, digits = 0)))



# evolución inscritos por edición---------------

p3 <- evol %>% 
  inner_join(evol1, by = "id") %>% 
  ggplot(aes(x = id, y = n, group = 1)) +
  geom_col(color = "aquamarine", fill = "aquamarine") +
  geom_path(color = "gray80", size = 1, linetype = "dotdash") +
  geom_text(aes(label = n, family = "Bahnschrift"), color = "blue", vjust = 0) +
  geom_hline(yintercept = 1056.167, color = "red", linetype = "dotdash") +
  annotate("text",label = "Media de inscritos por torneo: 1056", x = 2.3, y = 1110, 
           family = "Bahnschrift", color = "red", vjust = 1) +
  labs(
    title = "Evolución nº de inscritos por torneo",
    subtitle = "",
    caption = "",
    x = "Edición", 
    y = "Nº de inscritos") + 
  scale_x_discrete(breaks = seq(1, 6, 1), labels=c("1" = "I", 
                                                   "2" = "II",
                                                   "3" = "III",
                                                   "4" = "IV",
                                                   "5" = "V",
                                                   "6" = "VI")) +
  scale_y_continuous(breaks = seq(0, 1800, by = 200), limits = c(0,1800)) +
  my_theme +
  theme(axis.text = element_text(size = 10.5))


# evolucion rating medio ---------------

p5 <- evol %>% 
  inner_join(evol1, by = "id") %>% 
  ggplot(aes(x = id, y = mean_rating, group = 1)) +
  geom_col(color = "aquamarine", fill = "aquamarine") +
  geom_path(color = "gray80", size = 1, linetype = "dotdash") +
  geom_text(aes(label = round(mean_rating,0), family = "Bahnschrift"), color = "blue", vjust = 0) +
  geom_hline(yintercept = 1543.5, color = "red", linetype = "dotdash") +
  geom_hline(yintercept = 3015, color = "red", linetype = "dotdash") +
  annotate("text",label = "Rating medio por torneo: 1544", x = 4, y = 1523, 
           family = "Bahnschrift", color = "red", vjust = 1) +
  annotate("text",label = "Rating nº1 lichess.org GM Jepetto 3015", x = 3.3, y = 3000, 
             family = "Bahnschrift", color = "red", vjust = 1) + 
  labs(
    title = "Evolución Rating medio por torneo",
    subtitle = "",
    caption = "",
    x = "Edición", 
    y = "Nº de inscritos") + 
  scale_x_discrete(breaks = seq(1, 6, 1), labels=c("1" = "I", 
                                                   "2" = "II",
                                                   "3" = "III",
                                                   "4" = "IV",
                                                   "5" = "V",
                                                   "6" = "VI")) +
  scale_y_continuous(breaks = seq(0, 3100, by = 600), limits = c(0,3100)) +
  my_theme

# dispersion rating -----------------

p4 <- df %>% 
  ggplot(aes(x = id, y = Rating)) +
  geom_boxplot(fill = "gray95", color = "blue") +
  #geom_violin(fill = "aquamarine") +
  stat_summary(fun = mean, geom = "line", size = 2, color = "darkorange") +
  geom_jitter(alpha = 0.5, fill = "aquamarine", color = "aquamarine") +
  geom_text_repel(data=subset(df, Rating > 2560),
            aes(id,Rating,label=Username), color = "black",  family = "Bahnschrift") +
  geom_hline(yintercept = 1543.5, color = "red", linetype = "dashed") +
  geom_hline(yintercept = 3015, color = "red", linetype = "dashed") +
  annotate("text",label = "Rating medio por torneo: 1544", x = 4, y = 1523, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6) +
  annotate("text",label = "Rating Blitz nº1 lichess.org GM Jepetto 3015", x = 3.3, y = 3000, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6) + 
  labs(
    title = "Distribución Rating por torneo",
    subtitle = "",
    caption = "",
    x = "Edición", 
    y = "Rating de los participantes") + 
  scale_x_discrete(breaks = seq(1, 6, 1), labels=c("1" = "I", 
                                                   "2" = "II",
                                                   "3" = "III",
                                                   "4" = "IV",
                                                   "5" = "V",
                                                   "6" = "VI")) +
  scale_y_continuous(breaks = seq(600, 3100, by = 300), limits = c(600,3100)) +
  my_theme +
  theme(axis.text = element_text(size = 10.5))

# cuadro IV edicion -------------

p6 <- df4 %>% 
  filter(Rank <=10) %>% 
  ggplot(aes(x = reorder(Username, Rank), y = Points, fill = -Rank)) +
  #geom_tile(aes(fill = Tie.Break)) +
  geom_text(aes(label = Tie.Break), family = "Bahnschrift", color = "blue",
            vjust = -1, size = 3.6) +
  annotate("text", label = "Tie Break", x = 8, y = 11.9, 
           family = "Bahnschrift", color = "blue", vjust = 1, size = 2.6) +
  geom_col() +
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold"), name = "Score") +
  labs(
    title = "Top 10 IV Edición",
    subtitle = "Suizo 11 rondas",
    y = "Puntuación",
    x = ""
  ) +
  scale_y_continuous(breaks = seq(0,11, by = 2), limits = c(0,12)) +
  my_theme +
  theme(axis.text.x = element_text(size = 7)) +
  theme(legend.position = "none")









# bigchess -----------

data1 <- read.pgn("lichess_tournament_2021.04.30_UJyeH8oF_i-torneo-luisoners.pgn")
data2 <- read.pgn("lichess_tournament_2021.05.14_oyN6nfa4_ii-torneo-luisoners.pgn")
data3 <- read.pgn("lichess_tournament_2021.05.28_jBpvbJzr_iii-torneo-luisoners.pgn")
data4 <- read.pgn("lichess_swiss_2021.06.18_RooCpAzR_iv-torneo-luisoners.pgn")
data5 <- read.pgn("lichess_tournament_2021.07.16_NGYZktjj_v-torneo-luisoners.pgn")
data6 <- read.pgn("lichess_tournament_2021.08.27_NxCUTCny_vi-torneo-luisoners.pgn")

data <- bind_rows(data1, data2, data3, data4, data5, data6)



# jugadores con más partidas ----------------

sum_negras <- data %>% 
  group_by(Black) %>% 
  count(Black) %>% 
  arrange(-n) %>% 
  rename(negras = n) %>%
  rename(jugador = Black) 

sum_blancas <- data %>% 
  group_by(White) %>% 
  count(White) %>% 
  arrange(-n) %>% 
  rename(blancas = n) %>% 
  rename(jugador = White)

partidas_jugadas <- sum_negras %>% 
  left_join(sum_blancas, by = "jugador") %>% 
  mutate(partidas = sum(negras+blancas)) %>% 
  arrange(-partidas) 

p7 <- partidas_jugadas %>% 
  head(n = 10) %>% 
  ggplot(aes(x = reorder(jugador,-partidas), y = partidas,
             fill = partidas)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = partidas), vjust = 1, family = "Bahnschrift", color = "blue", size = 3.6) +
  labs(
    title = "Top 10 histórico jugadores con más partidas disputadas",
    subtitle = "",
    caption = "",
    x = "", 
    y = "Partidas disputadas"
  ) +
  my_theme +
  theme(legend.position = "none")

# quién ha sumado más victorias con blancas ---------------

victorias_con_blancas <-  data %>% 
  filter(Result == "1-0") %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(Result) %>% 
  rename(victorias_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-victorias_blancas) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(porcentaje_victorias_blancas = sum(victorias_blancas/partidas)*100) %>% 
  filter(partidas >= 50) %>% 
  arrange(-porcentaje_victorias_blancas)

mean(victorias_con_blancas$porcentaje_victorias_blancas)

p20 <- victorias_con_blancas %>%
  head(n = 10) %>% 
  ggplot(aes(x = reorder(jugador,porcentaje_victorias_blancas), y = porcentaje_victorias_blancas,
             fill = porcentaje_victorias_blancas)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold"), name = "") +
  geom_text(aes(label = round(porcentaje_victorias_blancas,1), hjust = 1, family = "Bahnschrift"), color = "blue", size = 3.6) +
  geom_hline(yintercept = mean(victorias_con_blancas$porcentaje_victorias_blancas), color = "red", linetype = "dashed") +
  annotate("text", label = "?? -- 25.6", x = 10, y = 12.5, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6, angle = 0) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "% Victorias con blancas"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()


# quién ha sumado más tablas con blancas -----------
tablas_con_blancas <- data %>% 
  filter(Result == "1/2-1/2") %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(Result) %>%
  rename(tablas_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-tablas_blancas) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(porcentaje_tablas_blancas = sum(tablas_blancas/partidas)*100) %>% 
  filter(partidas >= 50) %>% 
  arrange(-porcentaje_tablas_blancas) 

p21 <- tablas_con_blancas %>% 
  head(n = 10) %>% 
  ggplot(aes(x = reorder(jugador,porcentaje_tablas_blancas), y = porcentaje_tablas_blancas,
             fill = porcentaje_tablas_blancas)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(porcentaje_tablas_blancas,1), hjust = 1, family = "Bahnschrift"), color = "blue", size = 3.6) +
  geom_hline(yintercept = mean(tablas_con_blancas$porcentaje_tablas_blancas), color = "red", linetype = "dashed") +
  annotate("text", label = "3.23", x = 10, y = 4.3, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "% Tablas con blancas"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()


# quién ha sumado más derrotas con blancas --------------

derrotas_con_blancas <- data %>% 
  filter(Result == "0-1") %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(Result) %>% 
  rename(derrotas_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-derrotas_blancas) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(porcentaje_derrotas_blancas = sum(derrotas_blancas/partidas)*100) %>% 
  filter(partidas >= 50) %>% 
  arrange(-porcentaje_derrotas_blancas) 

p22 <- derrotas_con_blancas %>% 
  head(n = 10) %>% 
  ggplot(aes(x = reorder(jugador,porcentaje_derrotas_blancas), y = porcentaje_derrotas_blancas,
             fill = porcentaje_derrotas_blancas)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(porcentaje_derrotas_blancas,1), hjust = 1, family = "Bahnschrift"), color = "blue", size = 3.6) +
  geom_hline(yintercept = mean(derrotas_con_blancas$porcentaje_derrotas_blancas), color = "red", linetype = "dashed") +
  annotate("text", label = "22.48", x = 10, y = 15.8, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6, angle = 0) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "% Derrotas con blancas"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()

# quién ha sumado más victorias con negras ---------------
victorias_con_negras <- data %>% 
  filter(Result == "0-1") %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(Result) %>% 
  rename(victorias_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-victorias_negras) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(porcentaje_victorias_negras = sum(victorias_negras/partidas)*100) %>% 
  filter(partidas >= 50) %>% 
  arrange(-porcentaje_victorias_negras) 

mean(victorias_con_negras$porcentaje_victorias_negras)

p23 <- victorias_con_negras %>%
  head(n = 10) %>% 
  ggplot(aes(x = reorder(jugador,porcentaje_victorias_negras), y = porcentaje_victorias_negras,
             fill = porcentaje_victorias_negras)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(porcentaje_victorias_negras,1), hjust = 1, family = "Bahnschrift"), color = "blue", size = 3.6) +
  geom_hline(yintercept = mean(victorias_con_negras$porcentaje_victorias_negras), color = "red", linetype = "dashed") +
  annotate("text", label = "24.2", x = 10, y = 19.3, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "% Victorias con negras"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()





# quién ha sumado más tablas con negras ---------------

tablas_con_negras <- data %>% 
  filter(Result == "1/2-1/2") %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(Result) %>% 
  rename(tablas_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-tablas_negras) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(porcentaje_tablas_negras = sum(tablas_negras/partidas)*100) %>% 
  filter(partidas >= 50) %>% 
  arrange(-porcentaje_tablas_negras) 

p24 <- tablas_con_negras %>%
  head(n = 10) %>% 
  ggplot(aes(x = reorder(jugador,porcentaje_tablas_negras), y = porcentaje_tablas_negras,
             fill = porcentaje_tablas_negras)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(porcentaje_tablas_negras,1), hjust = 1, family = "Bahnschrift"), color = "blue", size = 3.6) +
  geom_hline(yintercept = mean(tablas_con_negras$porcentaje_tablas_negras), color = "red", linetype = "dashed") +
  annotate("text", label = "3.12", x = 10, y = 4, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6, angle = 0) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "% Tablas con negras"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()


# quién ha sumado más derrotas con negras --------------

derrotas_con_negras <- data %>% 
  filter(Result == "1-0") %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(Result) %>% 
  rename(derrotas_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-derrotas_negras) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(porcentaje_derrotas_negras = sum(derrotas_negras/partidas)*100) %>% 
  filter(partidas >= 50) %>% 
  arrange(-porcentaje_derrotas_negras) 

p25 <- derrotas_con_negras %>% 
  head(n = 10) %>% 
  ggplot(aes(x = reorder(jugador,porcentaje_derrotas_negras), y = porcentaje_derrotas_negras,
             fill = porcentaje_derrotas_negras)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(porcentaje_derrotas_negras,1), hjust = 1, family = "Bahnschrift"), color = "blue", size = 3.6) +
  geom_hline(yintercept = mean(derrotas_con_negras$porcentaje_derrotas_negras), color = "red", linetype = "dashed") +
  annotate("text", label = "23.2", x = 10, y = 17.65, 
           family = "Bahnschrift", color = "red", vjust = 1, size = 3.6, angle = 0) +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "% Derrotas con negras"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()
  



  
  


# jugador que mueve más la reina ---------------

blancas_reina <- data %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(wt = W_Q_moves) %>% 
  rename(mov_reina_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-mov_reina_blancas)

negras_reina <- data %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(wt = B_Q_moves) %>% 
  rename(mov_reina_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-mov_reina_negras)

movimientos_reina <- blancas_reina %>% 
  left_join(negras_reina, by = "jugador") %>% 
  mutate(movimientos = sum(mov_reina_blancas+mov_reina_negras)) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(movimientos_reina_porpartida = sum(movimientos/partidas)) %>% 
  filter(partidas >= 50) %>% 
  arrange(-movimientos_reina_porpartida) %>% 
  head(n = 10)

p10 <- movimientos_reina %>% 
  ggplot(aes(x = reorder(jugador, movimientos_reina_porpartida), y = movimientos_reina_porpartida,
             fill = movimientos_reina_porpartida)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(movimientos_reina_porpartida,1), hjust = 1, family = "Bahnschrift"), size = 3.6, color = "blue") +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "Reina"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()
  


# jugador que mueve mas el caballo -----------------

blancas_caballo <- data %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(wt = W_N_moves) %>% 
  rename(mov_caballo_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-mov_caballo_blancas)

negras_caballo <- data %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(wt = B_N_moves) %>% 
  rename(mov_caballo_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-mov_caballo_negras) 

movimientos_caballo <- blancas_caballo %>% 
  left_join(negras_caballo, by = "jugador") %>% 
  mutate(movimientos = sum(mov_caballo_blancas+mov_caballo_negras)) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(movimientos_caballo_porpartida = sum(movimientos/partidas)) %>% 
  filter(partidas >= 50) %>% 
  arrange(-movimientos_caballo_porpartida)  %>% 
  head(n = 10)


p11 <- movimientos_caballo %>% 
  ggplot(aes(x = reorder(jugador, movimientos_caballo_porpartida), y = movimientos_caballo_porpartida,
             fill = movimientos_caballo_porpartida)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(movimientos_caballo_porpartida,1), hjust = 1, family = "Bahnschrift"), size = 3.6, color = "blue") +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "Caballo"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()


# jugador que mueves mas la torre -------------------

blancas_torre <- data %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(wt = W_R_moves) %>% 
  rename(mov_torre_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-mov_torre_blancas)

negras_torre <- data %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(wt = B_R_moves) %>%
  rename(mov_torre_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-mov_torre_negras)
 
movimientos_torre <- blancas_torre %>% 
  left_join(negras_torre, by = "jugador") %>% 
  mutate(movimientos = sum(mov_torre_blancas+mov_torre_negras)) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(movimientos_torre_porpartida = sum(movimientos/partidas)) %>% 
  filter(partidas >= 50) %>% 
  arrange(-movimientos_torre_porpartida) %>% 
  head(n = 10)

p12 <- movimientos_torre %>% 
  ggplot(aes(x = reorder(jugador, movimientos_torre_porpartida), y = movimientos_torre_porpartida,
             fill = movimientos_torre_porpartida)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(movimientos_torre_porpartida,1), hjust = 1, family = "Bahnschrift"), size = 3.6, color = "blue") +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "Torre"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()



# jugador que mueve mas el alfil ----------------

blancas_alfil <- data %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(wt = W_B_moves) %>%
  rename(mov_alfil_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-mov_alfil_blancas)

negras_alfil <- data %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(wt = B_B_moves) %>%
  rename(mov_alfil_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-mov_alfil_negras)


movimientos_alfil <- blancas_alfil %>% 
  left_join(negras_alfil, by = "jugador") %>% 
  mutate(movimientos = sum(mov_alfil_blancas+mov_alfil_negras)) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(movimientos_alfil_porpartida = sum(movimientos/partidas)) %>% 
  filter(partidas >= 50) %>% 
  arrange(-movimientos_alfil_porpartida) %>% 
  head(n = 10)

p13 <- movimientos_alfil %>% 
  ggplot(aes(x = reorder(jugador, movimientos_alfil_porpartida), y = movimientos_alfil_porpartida,
             fill = movimientos_alfil_porpartida)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(movimientos_alfil_porpartida,1), hjust = 1, family = "Bahnschrift"), size = 3.6, color = "blue") +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "Alfil"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()




# jugador que mueve más el rey ------------

blancas_rey <- data %>% 
  select(-Black) %>% 
  group_by(White) %>% 
  count(wt = W_K_moves) %>%
  rename(mov_rey_blancas = n) %>% 
  rename(jugador = White) %>% 
  arrange(-mov_rey_blancas)
  

negras_rey <-  data %>% 
  select(-White) %>% 
  group_by(Black) %>% 
  count(wt = B_K_moves) %>% 
  rename(mov_rey_negras = n) %>% 
  rename(jugador = Black) %>% 
  arrange(-mov_rey_negras)

movimientos_rey <- blancas_rey %>% 
  left_join(negras_rey, by = "jugador") %>% 
  mutate(movimientos = sum(mov_rey_blancas+mov_rey_negras)) %>% 
  left_join(partidas_jugadas, by = "jugador") %>% 
  mutate(movimientos_rey_porpartida = sum(movimientos/partidas)) %>% 
  filter(partidas >= 50) %>% 
  arrange(-movimientos_rey_porpartida) %>% 
  head(n = 10)

p14 <- movimientos_rey %>% 
  ggplot(aes(x = reorder(jugador, movimientos_rey_porpartida), y = movimientos_rey_porpartida,
             fill = movimientos_rey_porpartida)) +
  geom_col() + 
  scale_fill_gradientn(colours = c("skyblue", "#cd7f32", "gold")) +
  geom_text(aes(label = round(movimientos_rey_porpartida,1), hjust = 1, family = "Bahnschrift"), size = 3.6, color = "blue") +
  labs(
    title = "",
    subtitle = "",
    caption = "",
    x = "", 
    y = "Rey"
  ) +
  my_theme +
  theme(legend.position = "none") +
  coord_flip()


# unir cuadro de honor  ----------------------

library(patchwork)
library(ragg)

pcuadrodehonor <- (p1| p6/p7/p2)

pcuadrodehonor <- pcuadrodehonor + plot_annotation(
  title = "",
  subtitle = '',
  caption = 'lichess.org | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

pcuadrodehonor <- pcuadrodehonor + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                             plot.title = element_text(color = "black", hjust = 0.5),
                                             plot.subtitle = element_text(color = "black", hjust = 0.5),
                                             #panel.background = element_rect(fill = "black", color = "black"),
                                             plot.caption = element_text(color = "black")))

pcuadrodehonor <- pcuadrodehonor + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

pcuadrodehonor <- pcuadrodehonor + plot_annotation(theme = theme(plot.title = element_text(size = 28, hjust = 0.5),
                                             plot.subtitle = element_text(size = 24, hjust = 0.5),
                                             plot.caption = element_text(size = 9.5, hjust = 1)))

ggsave("pcuadrodehonor.png", width = 17.5, height = 12, device = agg_png, dpi = 500)


# unir movimientos --------------

pmovimientos <- (p10|p11|p12|p13|p14)

pmovimientos <- pmovimientos + plot_annotation(
  title = "Más movimientos por partida según pieza",
  subtitle = 'Jugadores con 50 o más partidas disputadas',
  caption = 'lichess.org | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

pmovimientos <- pmovimientos + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                                                 plot.title = element_text(color = "black", hjust = 0.5),
                                                                 plot.subtitle = element_text(color = "black", hjust = 0.5),
                                                                 #panel.background = element_rect(fill = "black", color = "black"),
                                                                 plot.caption = element_text(color = "black")))

pmovimientos <- pmovimientos + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

pmovimientos <- pmovimientos + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                                                 plot.subtitle = element_text(size = 14, hjust = 0),
                                                                 plot.caption = element_text(size = 9.5, hjust = 1)))

ggsave("pmovimientos.png", width = 17.5, height = 12, device = agg_png, dpi = 500)


# evolucion torneo -------------

pevolucion <- (p3|p4)

pevolucion <- pevolucion + plot_annotation(
  title = "",
  subtitle = '',
  caption = 'lichess.org | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

pevolucion <- pevolucion + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                                             plot.title = element_text(color = "black", hjust = 0.5),
                                                             plot.subtitle = element_text(color = "black", hjust = 0.5),
                                                             panel.background = element_rect(fill = "black", color = "black"),
                                                             plot.caption = element_text(color = "black")))

pevolucion <- pevolucion + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

pevolucion <- pevolucion + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                                             plot.subtitle = element_text(size = 14, hjust = 0),
                                                             plot.caption = element_text(size = 9.5, hjust = 1)))

ggsave("pevolucion.png", width = 17.5, height = 12, device = agg_png, dpi = 500)

# resultados por color ---------------

presultados <- (p20|p21|p22|p23|p24|p25) 


presultados <- presultados + plot_annotation(
  title = "Top 10 porcentaje de resultados con blancas y negras",
  subtitle = 'Jugadores con 50 o más partidas',
  caption = 'lichess.org | @dataR_amateur') +
  theme(plot.caption = element_text(size = 9.5, hjust = 1))

presultados <- presultados + plot_annotation(theme = theme(plot.background = element_rect(fill  = 'gray95', color = "gray95"),
                                                         plot.title = element_text(color = "black", hjust = 0.5),
                                                         plot.subtitle = element_text(color = "black", hjust = 0.5),
                                                         panel.background = element_rect(fill = "black", color = "black"),
                                                         plot.caption = element_text(color = "black")))

presultados <- presultados + plot_annotation(theme = theme(text=element_text(family = "Bahnschrift", face = "bold")))

presultados <- presultados + plot_annotation(theme = theme(plot.title = element_text(size = 18, hjust = 0.5),
                                                         plot.subtitle = element_text(size = 14, hjust = 0),
                                                         plot.caption = element_text(size = 9.5, hjust = 1)))

ggsave("presultados.png", width = 17.5, height = 12, device = agg_png, dpi = 500)
