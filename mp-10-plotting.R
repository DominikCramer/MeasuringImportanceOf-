
library(tidyr)
library(dplyr)

setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

#import results dataframes
fa_importance_df <- read_csv2("../mtdc-mp-analysis/results-dfs/full_fa_importance_df.csv", col_types = cols())
fa_importance_rank_df <- read_csv2("../mtdc-mp-analysis/results-dfs/full_fa_importance_rank_df.csv", col_types = cols())
bfa_importance_df <- read_csv2("../mtdc-mp-analysis/results-dfs/full_bfa_importance_df.csv", col_types = cols())
bfa_importance_rank_df <- read_csv2("../mtdc-mp-analysis/results-dfs/full_bfa_importance_rank_df.csv", col_types = cols())


# define colours
hertie <- "#952220"
spd <- "#e3000f"
linke <- "#be3075"
gruene <- "#46962b"
fdp <- "#ffff00"
cdu <- "#000000"
afd <- "#009ee0"



# convert bfa_importance_rank_df to long format
df_long <- bfa_importance_df %>%
  gather(month, value, -article)
# create a new column with the year and month as a date object
df_long$date <- as.Date(paste0(df_long$month, "-01"))
# select and group by month and article, summarizing the values
df_monthly <- df_long %>%
  select(-month) %>%
  pivot_wider(names_from = article, values_from = value)



# plot for the rank of a single politician
ggplot(df_monthly, aes(date, Armin_Laschet)) +
  geom_line(color = hertie, size = 1) + geom_point(shape = 21, fill = "white", size = 1.5) +
  geom_smooth(color = "black") +
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")), 
               date_breaks = "1 year", 
               date_labels = "%b %Y") +
  scale_y_reverse() + # reverse y scale for rankings so that most important people are on top
  labs(x = "Date", y = "Rank", title = "Markus Söder") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# plot for the scores of two politicians
plot <- ggplot(df_monthly, aes(date)) +
#  geom_line(aes(y = Armin_Laschet), color = gruene, size = 0.3) +
  geom_smooth(aes(y = Armin_Laschet), color = gruene, size = 1, span = 0.25) +
  geom_text(aes(x = as.Date("2014-06-01"), y = 1.1, label = "Armin Laschet"), size = 5, color = gruene) +
  geom_point(aes(y = Armin_Laschet), shape = 21, fill = gruene, size = 1.5) +
#  geom_line(aes(y = Friedrich_Merz), color = cdu, size = 0.3) +
  geom_smooth(aes(y = Friedrich_Merz), color = cdu, size = 1, span = 0.25) +
  geom_text(aes(x = as.Date("2014-06-01"), y = 3.25, label = "Friedrich Merz"), size = 5, color = cdu) +
  # important events
  geom_point(aes(y = Friedrich_Merz), shape = 24, fill = cdu, size = 1.5) +
  geom_vline(aes(xintercept = as.Date("2012-06-30")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2012-09-10"), y = 0.8, label = "(1)"), size = 5) + # Laschet wird CDU Landeschef
  geom_vline(aes(xintercept = as.Date("2017-06-27")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2017-09-07"), y = 0.8, label = "(2)"), size = 5) + # Laschet wird NW Ministerpräsident
  geom_vline(aes(xintercept = as.Date("2018-10-30")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2019-01-10"), y = 0.8, label = "(3)"), size = 5) + # Merz kündigt Kandidatur für CDU Vorsitz an
  geom_vline(aes(xintercept = as.Date("2021-01-22")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2021-04-02"), y = 0.8, label = "(4)"), size = 5) + # Laschet wird gewinnt gegen merz, wird CDU chef
  geom_vline(aes(xintercept = as.Date("2022-01-31")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2022-04-11"), y = 0.8, label = "(5)"), size = 5) + # Merz wird CDU Chef
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%b %Y") +
#  scale_y_reverse() + # reverse y scale for rankings so that most important people are on top
  labs(x = "Date", y = "Score", title = "Bayesian Factor Scores of Armin Laschet and Friedrich Merz over Time") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../mtdc-mp-analysis/figures/laschet-merz-bfa.jpg", plot, width = 9.4, height = 6.7, dpi = 300)



## --- trying out small multiples ---
df_m <- df_monthly %>% select(date,
                              # Olaf_Scholz,
                              # Gregor_Gysi,
                              Armin_Laschet,
                              Friedrich_Merz,
                              # Thomas_Kemmerich,
                              Sahra_Wagenknecht,
                              Alexander_Gauland,
                              `Bj%C3%B6rn_H%C3%B6cke`,
                              # `Marie-Agnes_Strack-Zimmermann`,
                              # `Katrin_G%C3%B6ring-Eckardt`,
                              # Franziska_Giffey,
                              Boris_Pistorius,
                              # Klaus_Ernst,
                              # Hubert_Aiwanger,
                              Jens_Spahn,
                              # Tino_Chrupalla,
                              Alice_Weidel,
                              Saskia_Esken,
                              Lars_Klingbeil,
                              Omid_Nouripour,
                              Ricarda_Lang,
                              Christian_Lindner,
                              Janine_Wissler,
                              # Robert_Habeck,
                              Annalena_Baerbock,
                              `Cem_%C3%96zdemir`
                              )

names(df_m) <- c("date",
               # "Olaf Scholz (SPD)",
               # "Gregor Gysi",
               "Armin Laschet (CDU)",
               "Friedrich Merz (CDU)",
               # "Thomas Kemmerich",
               "Sahra Wagenknecht (Linke)",
               "Alexander Gauland (AfD)",
               "Björn Höcke (AfD)",
               # "Marie-Agnes Strack-Zimmermann",
               # "Katrin Göring-Eckardt",
               # "Franziska Giffey",
               "Boris Pistorius (SPD)",
               # "Klaus Ernst",
               # "Hubert Aiwanger",
               "Jens Spahn (CDU)",
               # "Tino Chrupalla (AfD)",
               "Alice Weidel (AfD)",
               "Saskia Esken (SPD)",
               "Lars Klingbeil (SPD)",
               "Omid Nouripour (Grüne)",
               "Ricarda Lang (Grüne)",
               "Christian Lindner (FDP)",
               "Janine Wissler (Linke)",
               # "Robert Habeck",
               "Annalena Baerbock (Grüne)",
               "Cem Özdemir (Grüne)"
               )

df_longer <- df_m %>%
  pivot_longer(cols = -date, names_to = "politician", values_to = "ranking")

plot <- ggplot(df_longer, aes(x = date, y = ranking)) +
  geom_point(shape = 16, fill = "white", size = 0.3) +
  geom_smooth(color = hertie, size = 0.5, se = FALSE, span = 0.25) +
  facet_wrap(~politician, scales = "free_y") +
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y") +
  ylim(-2, 6) + # for bfa
#  scale_y_reverse(limits = c(100, 1)) + # for bfa_rank & fa_rank - reverse y scale so that most important people are on top
  labs(x = "Date", y = "Score", title = "Bayesian Factor Scores of Politicians over Time") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 5.5),
        strip.text = element_text(size = 6))

ggsave("../mtdc-mp-analysis/figures/notables-bfa.jpg", plot, width = 7, height = 5, dpi = 300)




# small multiples regierungschefs (andreas bovenschulte und peter tschentscher und malu_dreyer sind kein mp mehr wegen ämtertrennung -> next time, include former mps)
df_m <- df_monthly %>% select(date,
                              Olaf_Scholz,
                              Winfried_Kretschmann,
                              `Markus_S%C3%B6der`,
                              Kai_Wegner,
                              Dietmar_Woidke,
                              Boris_Rhein,
                              Manuela_Schwesig,
                              Stephan_Weil,
                              `Hendrik_W%C3%BCst`,
                              Anke_Rehlinger,
                              Michael_Kretschmer,
                              Reiner_Haseloff,
                              `Daniel_G%C3%BCnther`,
                              Bodo_Ramelow
                              )

names(df_m) <- c("date",
                 "Olaf Scholz (SPD, DE)",
                 "Winfried Kretschmann (Grüne, BW)",
                 "Markus Söder (CSU, BY)",
                 "Kai Wegner (CDU, BE)",
                 "Dietmar Woidke (SPD, BB)",
                 "Boris Rhein (CDU, HE)",
                 "Manuela Schwesig (SPD, MV)",
                 "Stephan Weil (SPD, NI)",
                 "Hendrik Wüst (CDU, NW)",
                 "Anke Rehlinger (SPD, SL)",
                 "Michael Kretschmer (CDU, SN)",
                 "Reiner Haseloff (CDU, ST)",
                 "Daniel Günther (CDU, SH)",
                 "Bodo Ramelow (Linke, TH)"
                 )

df_longer <- df_m %>%
  pivot_longer(cols = -date, names_to = "politician", values_to = "ranking")

plot <- ggplot(df_longer, aes(x = date, y = ranking)) +
  geom_point(shape = 16, fill = "white", size = 0.3) +
  geom_smooth(color = hertie, size = 0.5, se = FALSE, span = 0.25) +
  facet_wrap(~politician, scales = "free_y") +
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y") +
  ylim(-0.75, 5.75) + # for bfa
  #  scale_y_reverse(limits = c(100, 1)) + # for bfa_rank & fa_rank - reverse y scale so that most important people are on top
  labs(x = "Date", y = "Score", title = "Bayesian Factor Scores of Heads of Government over Time") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 5.5),
        strip.text = element_text(size = 6))

ggsave("../mtdc-mp-analysis/figures/regierungschefs-bfa.jpg", plot, width = 7, height = 5, dpi = 300)
