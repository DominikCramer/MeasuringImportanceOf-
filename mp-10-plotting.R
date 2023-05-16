## this script is still pretty messy

library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")

#import results dataframes
# fa_importance_df <- read_csv2("../mtdc-mp-analysis/results-dfs/full_fa_importance_df.csv", col_types = cols())
# fa_importance_rank_df <- read_csv2("../mtdc-mp-analysis/results-dfs/full_fa_importance_rank_df.csv", col_types = cols())
 bfa_importance_df <- read_csv2("/Users/DominikCramer/Desktop/master-thesis/MeasuringImportanceOfGermanPoliticalElites/data/full_bfa_importance_df.csv", col_types = cols())
# bfa_importance_rank_df <- read_csv2("../mtdc-mp-analysis/results-dfs/full_bfa_importance_rank_df.csv", col_types = cols())
load("/Users/DominikCramer/Desktop/master-thesis/MeasuringImportanceOfGermanPoliticalElites/data/fixed_mp_all_df.RData")

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

# calculate monthly mean
row_means <- rowMeans(select(df_monthly, -date), na.rm = TRUE)
df_monthly_mean <- cbind(df_monthly, row_means)
# plot mean over time
ggplot(df_monthly_mean, aes(date)) +
  geom_point(aes(y = row_means))


parliament_df <- select(fixed_mp_all_df, parliament, article)
bfa_parliament_df <- left_join(bfa_importance_df, parliament_df, by = "article")
# convert bfa_importance_rank_df to long format
df_long_p <- bfa_parliament_df %>%
  gather(month, value, -article, -parliament)
# create a new column with the year and month as a date object
df_long_p$date <- as.Date(paste0(df_long_p$month, "-01"))
# select and group by month and article, summarizing the values
df_monthly_p <- df_long_p %>%
  select(-month) %>%
  pivot_wider(names_from = parliament, values_from = value)
# remove article column
df_monthly_p <- select(df_monthly_p, -article)
# proof of principle with example berlin
be_df <- select(df_monthly_p, date, `Abgeordnetenhaus von Berlin`)
be_summary <- be_df %>%
  group_by(date) %>%
  summarize(mean_value = mean(`Abgeordnetenhaus von Berlin`, na.rm = TRUE))

df_summary <- df_monthly_p %>%
  group_by(date) %>%
  summarize_at(vars(`Abgeordnetenhaus von Berlin`, `Bayerischer Landtag`, `Bremische Bürgerschaft`, `Bürgerschaft der Freien und Hansestadt Hamburg`, `Deutscher Bundestag`, `Hessischer Landtag`, `Landtag Brandenburg`, `Landtag des Saarlandes`, `Landtag Mecklenburg-Vorpommern`, `Landtag Niedersachsen`, `Landtag Nordrhein-Westphalen`, `Landtag Rheinland-Pfalz`, `Landtag von Baden-Württemberg`, `Landtag von Sachsen-Anhalt`, `Sächsischer Landtag`, `Schleswig-Holsteinischer Landtag`, `Thüringer Landtag`), mean, na.rm = TRUE)

ggplot(df_summary, aes(date)) +
  geom_point(aes(y = `Deutscher Bundestag`))

df_longest <- df_summary %>%
  pivot_longer(cols = -date, names_to = "parliament", values_to = "mean")

# plot small multiples of mean in all 17 parliaments over time
plot <- ggplot(df_longest, aes(x = date, y = mean)) +
  geom_point(shape = 16, fill = "white", size = 0.3) +
  geom_smooth(color = hertie, size = 0.5, se = FALSE, span = 0.25) +
  facet_wrap(~parliament, scales = "free_y") +
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y") +
  ylim(-0.8, 1.1) + # for bfa
  #  scale_y_reverse(limits = c(100, 1)) + # for bfa_rank & fa_rank - reverse y scale so that most important people are on top
  labs(x = "Date", y = "Mean Score", title = "Mean Bayesian Factor Scores of Politicians over Time") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 5.5),
        strip.text = element_text(size = 6))

ggsave("../mtdc-mp-analysis/figures/mean-parliaments-bfa.jpg", plot, width = 7, height = 5, dpi = 300)


# plot for the mean score of members of the bundestag
plot <- ggplot(df_summary, aes(date, `Deutscher Bundestag`)) +
  # geom_line(color = hertie, size = 1) + 
  geom_point(shape = 21, fill = "white", size = 1.5) +
  geom_smooth(color = hertie, span = 0.25) +
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%b %Y") +
#  scale_y_reverse() + # reverse y scale for rankings so that most important people are on top
  labs(x = "Date", y = "Mean Score", title = "Mean Bayesian Factor Scores of Members of the Bundestag over Time") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../mtdc-mp-analysis/figures/mean-bundestag-bfa.jpg", plot, width = 7, height = 5, dpi = 300)

# comparing parliaments as of march 2023
parliaments_03_23 <- filter(df_summary, date == "2023-03-01") %>% pivot_longer(cols = -date) %>% select(-date)
#add column with inhabitants
parliaments_03_23$people <- as.numeric(c("3.7", "13.2", "0.7", "1.9", "83.2", "6.3", "2.5", "1.0", "1.6", "8.0", "17.9", "4.1", "11.1", "2.2", "4.0", "2.9", "2.1"))
# define factor level to have the chart sorted by value
parliaments_03_23$name <- factor(parliaments_03_23$name, levels = c("Deutscher Bundestag",
                                                                    "Abgeordnetenhaus von Berlin",
                                                                    "Bayerischer Landtag",
                                                                    "Landtag von Baden-Württemberg",
                                                                    "Thüringer Landtag",
                                                                    "Landtag Nordrhein-Westphalen",
                                                                    "Sächsischer Landtag",
                                                                    "Schleswig-Holsteinischer Landtag",
                                                                    "Landtag Niedersachsen",
                                                                    "Landtag Rheinland-Pfalz",
                                                                    "Hessischer Landtag",
                                                                    "Landtag Brandenburg",
                                                                    "Landtag Mecklenburg-Vorpommern",
                                                                    "Bremische Bürgerschaft",
                                                                    "Landtag von Sachsen-Anhalt",
                                                                    "Bürgerschaft der Freien und Hansestadt Hamburg",
                                                                    "Landtag des Saarlandes"
                                                                    ))
# only scores
ggplot(sorted_parliaments_03_23, aes(x = name, y = value)) +
  geom_bar(stat = "identity", fill = hertie, aes(x = reorder(name, -value))) +
  labs(x = "Parliament", y = "Mean Score of Members", title = "03 2023") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1))

#scores vs number of inhabitants
ggplot(parliaments_03_23, aes(x = reorder(name, -value))) +
  geom_bar(aes(y = people), stat = "identity", fill = "grey") +
  geom_bar(aes(y = value), stat = "identity", fill = hertie) +
  scale_y_continuous(name = "Value", sec.axis = sec_axis(~ ., name = "People"))

ggplot(parliaments_03_23, aes(x = name, y = value)) + 
  geom_bar(stat = "identity")

# # the following works!
# plot <- ggplot(parliaments_03_23, aes(x = reorder(name, -value))) +
#   geom_bar(aes(y = value), stat = "identity", fill = hertie) +
#   geom_line(aes(y = people/100, group =1), color = "black") +
#   scale_y_continuous(
#     name = "Mean Score of MPs",
#     sec.axis = sec_axis(~.*100, name = "Constituents (millions)")) +
#   labs(x = "", y = "Mean Score of Members") +
#   theme_bw() +
#   theme(panel.grid.minor = element_blank(),
#         axis.text.x = element_text(angle = 60, hjust = 1))
# 
# ggsave("../mtdc-mp-analysis/figures/score-v-people-bfa.jpg", plot, width = 7, height = 5, dpi = 300)
# 

# # plot for the rank of a single politician
# ggplot(df_monthly, aes(date, Armin_Laschet)) +
#   geom_line(color = hertie, size = 1) + geom_point(shape = 21, fill = "white", size = 1.5) +
#   geom_smooth(color = "black") +
#   scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")), 
#                date_breaks = "1 year", 
#                date_labels = "%b %Y") +
#   scale_y_reverse() + # reverse y scale for rankings so that most important people are on top
#   labs(x = "Date", y = "Rank", title = "Markus Söder") +
#   theme_bw() +
#   theme(panel.grid.minor = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))

# plot for the scores of two politicians
plot <- ggplot(df_monthly, aes(date)) +
#  geom_line(aes(y = Armin_Laschet), color = gruene, size = 0.3) +
  geom_smooth(aes(y = Armin_Laschet), color = "#CA908F", size = 1, span = 0.25, se = FALSE) +
  geom_text(aes(x = as.Date("2014-06-01"), y = 1.1, label = "Armin Laschet"), size = 4, color = "#CA908F") +
  geom_point(aes(y = Armin_Laschet), shape = 21, fill = "#CA908F", size = 1.5) +
#  geom_line(aes(y = Friedrich_Merz), color = cdu, size = 0.3) +
  geom_smooth(aes(y = Friedrich_Merz), color = hertie, size = 1, span = 0.25, se = FALSE) +
  geom_text(aes(x = as.Date("2014-06-01"), y = 3.15, label = "Friedrich Merz"), size = 4, color = hertie) +
  geom_point(aes(y = Friedrich_Merz), shape = 24, fill = hertie, size = 1.5) +
  # important events
  geom_vline(aes(xintercept = as.Date("2012-06-30")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2012-10-10"), y = 0.8, label = "(1)"), size = 4) + # Laschet wird CDU Landeschef
  geom_vline(aes(xintercept = as.Date("2017-06-27")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2017-10-07"), y = 0.8, label = "(2)"), size = 4) + # Laschet wird NW Ministerpräsident
  geom_vline(aes(xintercept = as.Date("2018-10-30")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2019-02-10"), y = 0.8, label = "(3)"), size = 4) + # Merz kündigt Kandidatur für CDU Vorsitz an
  geom_vline(aes(xintercept = as.Date("2021-01-22")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2021-05-02"), y = 0.8, label = "(4)"), size = 4) + # Laschet wird gewinnt gegen merz, wird CDU chef
  geom_vline(aes(xintercept = as.Date("2021-09-26")), linetype = "dashed") +
  geom_text(aes(x = as.Date("2022-01-05"), y = 0.8, label = "(5)"), size = 4) + # Buntestagswahl 2021
  # geom_vline(aes(xintercept = as.Date("2022-01-31")), linetype = "dashed") +
  # geom_text(aes(x = as.Date("2022-06-11"), y = 0.8, label = "(6)"), size = 4) + # Merz wird CDU Chef
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%b %Y") +
#  scale_y_reverse() + # reverse y scale for rankings so that most important people are on top
  labs(x = "Date", y = "Score") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("../mtdc-mp-analysis/figures/laschet-merz3-bfa.jpg", plot, width = 7, height = 5, dpi = 300)



## --- trying out small multiples ---
df_m <- df_monthly %>% select(date,
                              # Olaf_Scholz,
                              # Gregor_Gysi,
                              # Armin_Laschet,
                              # Friedrich_Merz,
                              Karl_Lauterbach,
                              # Ilse_Aigner,
                              # Serpil_Midyatli,
                              # `Thomas_Losse-M%C3%BCller`,
                              # Karin_Prien,
                              # `Serap_G%C3%BCler`,
                              Mona_Neubaur,
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
                              # Lars_Klingbeil,
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
               # "Armin Laschet (CDU)",
               # "Friedrich Merz (CDU)",
               "Karl Lauterbach (SPD)",
               # "Ilse Aigner (CSU)",
               # "Serpil Midyatli (SPD)",
               # "Thomas Losse-Müller (SPD)",
               # "Karin Prien (CDU)",
               # "Serap Güler (CDU)",
               "Mona Neubaur (Grüne)",
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
               # "Lars Klingbeil (SPD)",
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

 plot <-
  ggplot(df_longer, aes(x = date, y = ranking)) +
  geom_point(shape = 16, fill = "white", size = 0.3) +
  geom_smooth(color = hertie, size = 0.5, se = FALSE, span = 0.25) +
  facet_wrap(~politician, scales = "free_y", ncol = 3) +
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y") +
  ylim(-2, 6) + # for bfa
#  scale_y_reverse(limits = c(100, 1)) + # for bfa_rank & fa_rank - reverse y scale so that most important people are on top
  labs(x = "", y = "Score") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 5.5),
        strip.text = element_text(size = 6))

ggsave("../mtdc-mp-analysis/figures/notables90-bfa.jpg", plot, width = 5, height = 7, dpi = 300)




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

plot <-
  ggplot(df_longer, aes(x = date, y = ranking)) +
  geom_point(shape = 16, fill = "white", size = 0.3) +
  geom_smooth(color = hertie, size = 0.5, se = FALSE, span = 0.25) +
  facet_wrap(~politician, scales = "free_y", ncol = 3) +
  scale_x_date(limits = as.Date(c("2010-01-01", "2023-03-31")),
               date_breaks = "1 year",
               date_labels = "%Y") +
  ylim(-0.75, 5.75) + # for bfa
  #  scale_y_reverse(limits = c(100, 1)) + # for bfa_rank & fa_rank - reverse y scale so that most important people are on top
  labs(x = "", y = "Score") +
  theme_bw() +
  theme(panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 5.5),
        axis.text.y = element_text(size = 5.5),
        strip.text = element_text(size = 6))

ggsave("../mtdc-mp-analysis/figures/regierungschefs90-bfa.jpg", plot, width = 5, height = 7, dpi = 300)
