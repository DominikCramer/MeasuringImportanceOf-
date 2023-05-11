# Measuring the Importance of German Political Elites - 01

library(rvest)
library(dplyr)

# set working directory
setwd("/Users/DominikCramer/Desktop/master-thesis/mtdc-mp-analysis/")



# --------- scraping the links to all German MPs from lists of members of the 17 parliaments on Wikipedia -------------

page <- read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Deutschen_Bundestages_(20._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Deutscher Bundestag"
mp_de_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtags_von_Baden-W%C3%BCrttemberg_(17._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag von Baden-Württemberg"
mp_bw_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Bayerischen_Landtags_(18._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[2] %>% html_nodes("td:nth-child(1) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from SECOND table, FIRST column
name <- page %>% html_nodes("table") %>% .[2] %>% html_nodes("td:nth-child(1) a") %>% html_text("a")
parliament <- "Bayerischer Landtag"
mp_by_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)
mp_by_df <- subset(mp_by_df, wikiurl != "https://de.wikipedia.org/wiki/Tessa_Ganserer") # had to remove one entry manually because the MP had left parliament but Wikipedia was not up to date

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Abgeordnetenhauses_von_Berlin_(19._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[1] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from FIRST table, SECOND column
name <- page %>% html_nodes("table") %>% .[1] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Abgeordnetenhaus von Berlin"
mp_be_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtags_Brandenburg_(7._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[2] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from SECOND table, SECOND column
name <- page %>% html_nodes("table") %>% .[2] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag Brandenburg"
mp_bb_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_der_Bremischen_B%C3%BCrgerschaft_(20._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[1] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from FIRST table, SECOND column
name <- page %>% html_nodes("table") %>% .[1] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Bremische Bürgerschaft"
mp_hb_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_der_Hamburgischen_B%C3%BCrgerschaft_(22._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Bürgerschaft der Freien und Hansestadt Hamburg"
mp_hh_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Hessischen_Landtags_(20._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(1) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, FIRST column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(1) a") %>% html_text("a")
parliament <- "Hessischer Landtag"
mp_he_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtages_Mecklenburg-Vorpommern_(8._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag Mecklenburg-Vorpommern"
mp_mv_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)
mp_mv_df <- subset(mp_mv_df, wikiurl != "https://de.wikipedia.org/wiki/Elisabeth_A%C3%9Fmann") # had to remove one entry manually because the MP had left parliament but Wikipedia was not up to date

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Nieders%C3%A4chsischen_Landtages_(18._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag Niedersachsen"
mp_ni_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)
mp_ni_df <- subset(mp_ni_df, wikiurl != "https://de.wikipedia.org/wiki/Gabriele_Andretta") # had to remove one entry manually because the MP had left parliament but Wikipedia was not up to date


page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtages_Nordrhein-Westfalen_(18._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[4] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from FORTH table, SECOND column
name <- page %>% html_nodes("table") %>% .[4] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag Nordrhein-Westphalen"
mp_nw_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtages_Rheinland-Pfalz_(18._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag Rheinland-Pfalz"
mp_rp_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtages_des_Saarlandes_(17._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[4] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from FORTH table, SECOND column
name <- page %>% html_nodes("table") %>% .[4] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag des Saarlandes"
mp_sl_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_S%C3%A4chsischen_Landtags_(7._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Sächsischer Landtag"
mp_sn_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtages_von_Sachsen-Anhalt_(8._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from FORTH table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Landtag von Sachsen-Anhalt"
mp_st_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Landtages_Schleswig-Holstein_(20._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[1] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from FIRST table, SECOND column
name <- page %>% html_nodes("table") %>% .[1] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Schleswig-Holsteinischer Landtag"
mp_sh_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)

page = read_html("https://de.wikipedia.org/wiki/Liste_der_Mitglieder_des_Th%C3%BCringer_Landtags_(7._Wahlperiode)")
wikiurl <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_attr("href") %>% paste0("https://de.wikipedia.org", .) #grabing the article links from THIRD table, SECOND column
name <- page %>% html_nodes("table") %>% .[3] %>% html_nodes("td:nth-child(2) a") %>% html_text("a")
parliament <- "Thüringer Landtag"
mp_th_df <- data.frame(name, wikiurl, parliament, stringsAsFactors = FALSE)



# --------- combining all links into one dataframe -----------
mp_all_df <- bind_rows(mp_de_df, mp_bw_df, mp_by_df, mp_be_df, mp_bb_df, mp_hb_df, mp_hh_df, mp_he_df, mp_mv_df, mp_ni_df, mp_nw_df, mp_rp_df, mp_sl_df, mp_sn_df, mp_st_df, mp_sh_df, mp_th_df)
mp_all_df$article <- basename(mp_all_df$wikiurl)
save(mp_all_df,  file = "../mtdc-mp-analysis/mp_all_df.RData")



# manually fixing links which redirect to the correct article (e.g., because title uses a nickname or second name)
# this is necessary only for downloading the articles because the MediaWiki API cannot handle redirects (by contrast: Wikimedia REST API had no problem)
load("../mtdc-mp-analysis/mp_all_df.RData")
fixed_mp_all_df <- mp_all_df

fixed_mp_all_df[1637, "wikiurl"] <- "https://de.wikipedia.org/wiki/Ralph_Alexander_Lorz"
fixed_mp_all_df[1637, "article"] <- "Ralph_Alexander_Lorz"

fixed_mp_all_df[1541, "wikiurl"] <- "https://de.wikipedia.org/wiki/Andr%C3%A9_Trepoll"
fixed_mp_all_df[1541, "article"] <- "Andr%C3%A9_Trepoll"

fixed_mp_all_df[1580, "wikiurl"] <- "https://de.wikipedia.org/wiki/Angela_Dorn"
fixed_mp_all_df[1580, "article"] <- "Angela_Dorn"

fixed_mp_all_df[1082, "wikiurl"] <- "https://de.wikipedia.org/wiki/Anna_Schwamberger"
fixed_mp_all_df[1082, "article"] <- "Anna_Schwamberger"
fixed_mp_all_df[1082, "name"] <- "Anna Schwamberger"

fixed_mp_all_df[1793, "wikiurl"] <- "https://de.wikipedia.org/wiki/Bernd_Busemann"
fixed_mp_all_df[1793, "article"] <- "Bernd_Busemann"

fixed_mp_all_df[2497, "wikiurl"] <- "https://de.wikipedia.org/wiki/Eka_von_Kalben"
fixed_mp_all_df[2497, "article"] <- "Eka_von_Kalben"

fixed_mp_all_df[2341, "wikiurl"] <- "https://de.wikipedia.org/wiki/Frank_Richter_(Politiker)"
fixed_mp_all_df[2341, "article"] <- "Frank_Richter_(Politiker)"

fixed_mp_all_df[279, "wikiurl"] <- "https://de.wikipedia.org/wiki/Gyde_Jensen"
fixed_mp_all_df[279, "article"] <- "Gyde_Jensen"

fixed_mp_all_df[1359, "wikiurl"] <- "https://de.wikipedia.org/wiki/Janina_Strelow"
fixed_mp_all_df[1359, "article"] <- "Janina_Strelow"
fixed_mp_all_df[1359, "name"] <- "Janina Strelow"

fixed_mp_all_df[2337, "wikiurl"] <- "https://de.wikipedia.org/wiki/Juliane_Pfeil"
fixed_mp_all_df[2337, "article"] <- "Juliane_Pfeil"
fixed_mp_all_df[2337, "name"] <- "Juliane Pfeil"

fixed_mp_all_df[2408, "wikiurl"] <- "https://de.wikipedia.org/wiki/Lydia_H%C3%BCskens"
fixed_mp_all_df[2408, "article"] <- "Lydia_H%C3%BCskens"

fixed_mp_all_df[1363, "wikiurl"] <- "https://de.wikipedia.org/wiki/S%C3%BClmez_%C3%87olak"
fixed_mp_all_df[1363, "article"] <- "S%C3%BClmez_%C3%87olak"
fixed_mp_all_df[1363, "name"] <- "Sülmez Çolak"

fixed_mp_all_df[2405, "wikiurl"] <- "https://de.wikipedia.org/wiki/Sandra_Hietel-Heuer"
fixed_mp_all_df[2405, "article"] <- "Sandra_Hietel-Heuer"
fixed_mp_all_df[2405, "name"] <- "Sandra Hietel-Heuer"

fixed_mp_all_df[1445, "wikiurl"] <- "https://de.wikipedia.org/wiki/Sina_Aylin_Koriath"
fixed_mp_all_df[1445, "article"] <- "Sina_Aylin_Koriath"
fixed_mp_all_df[1445, "name"] <- "Sina Aylin Koriath"

fixed_mp_all_df[2575, "wikiurl"] <- "https://de.wikipedia.org/wiki/Thomas_Kemmerich"
fixed_mp_all_df[2575, "article"] <- "Thomas_Kemmerich"

fixed_mp_all_df[1834, "wikiurl"] <- "https://de.wikipedia.org/wiki/Veronika_Bode"
fixed_mp_all_df[1834, "article"] <- "Veronika_Bode"


save(fixed_mp_all_df,  file = "../mtdc-mp-analysis/fixed_mp_all_df.RData")
