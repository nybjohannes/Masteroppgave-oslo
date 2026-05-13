library(tidyverse)
library(readxl)
library(dplyr)
library(stargazer)




valgdeltakelse <- read_excel("data/valgdeltakelse.xlsx")
utdanning <- read_excel("data/utdanning.xlsx")
innvandring <- read_excel("data/innvandring.xlsx")

colnames(valgdeltakelse)
colnames(utdanning)
colnames(innvandring)

str(valgdeltakelse)



# Hvis geo_nivå finnes: beholder jeg kun bydel
if("geo_nivå" %in% names(valgdeltakelse)) {
  valgdeltakelse <- valgdeltakelse %>% filter(geo_nivå == "bydel")
}
if("geo_nivå" %in% names(utdanning)) {
  utdanning <- utdanning %>% filter(geo_nivå == "bydel")
}
if("geo_nivå" %in% names(innvandring)) {
  innvandring <- innvandring %>% filter(geo_nivå == "bydel")
}

# Filtrerer på år
valgdeltakelse <- valgdeltakelse %>% filter(år >= 2001, år <= 2025)
utdanning      <- utdanning %>% filter(år >= 2001, år <= 2025)
innvandring    <- innvandring %>% filter(år >= 2001, år <= 2025)


valgdeltakelse_w <- valgdeltakelse %>%
  select(geo_enhet, år, verdi) %>%
  rename(valgdeltakelse = verdi)

utdanning_w <- utdanning %>%
  select(geo_enhet, år, verdi) %>%
  rename(andel_hoyere_utd = verdi)

innvandring_w <- innvandring %>%
  select(geo_enhet, år, verdi) %>%
  rename(innvandrerandel = verdi)

#da kan jeg lage det første panelet

panel_2001 <- valgdeltakelse_w %>%
  left_join(utdanning_w,   by = c("geo_enhet", "år")) %>%
  left_join(innvandring_w, by = c("geo_enhet", "år"))


# sjekker det ut
head(panel_2001)
summary(panel_2001)

# Hvor mange mangler er det?
colSums(is.na(panel_2001))

# Sjekker om det finnes duplikater
panel_2001 %>%
  count(geo_enhet, år) %>%
  filter(n > 1)

#da går jeg videre, og lager panel også med inntekt

inntekt <- read_excel("inntekt.xlsx")
if("geo_nivå" %in% names(inntekt)) {
  inntekt <- inntekt %>% filter(geo_nivå == "bydel")
}
inntekt <- inntekt %>% filter(år >= 2005, år <= 2025)

inntekt_w <- inntekt %>%
  select(geo_enhet, år, verdi) %>%
  rename(inntekt = verdi)

panel_2005 <- panel_2001 %>%
  filter(år >= 2005) %>%
  left_join(inntekt_w, by = c("geo_enhet", "år"))

colSums(is.na(panel_2005))


colSums(is.na(panel_2001))

panel_2001 %>% count(geo_enhet, år) %>% filter(n > 1)

colSums(is.na(panel_2005))



repr_bydel_ar <- readRDS("data/bydel_ar_repr_allpartier_pop.rds")

glimpse(repr_bydel_ar)
names(repr_bydel_ar)
head(repr_bydel_ar)

repr_bydel_ar_clean <- repr_bydel_ar %>%
  filter(!is.na(bydel)) %>%     # fjern aggregater
  filter(ar >= 2001) %>%        # start analyseperiode
  filter(ar != 2003)            # fjern 2003

repr_bydel_ar_clean <- repr_bydel_ar_clean %>%
  mutate(
    put_per_10000 = (n_kandidater_put / befolkning) * 10000,
    nom_per_10000 = (n_kandidater_nom / befolkning) * 10000
  )

summary(repr_bydel_ar_clean$put_per_10000)



repr_merge <- repr_bydel_ar_clean %>%
  rename(
    geo_enhet = bydel,
    år = ar
  ) %>%
  select(geo_enhet, år, put_per_10000, nom_per_10000)

panel_2001 <- panel_2001 %>%
  filter(år != 2003) %>%
  left_join(repr_merge, by = c("geo_enhet", "år"))

panel_2005 <- panel_2005 %>%
  filter(år != 2003) %>%
  left_join(repr_merge, by = c("geo_enhet", "år"))



# valgtype-dummy (storting vs kommune)
stortingsaar <- c(2001, 2005, 2009, 2013, 2017, 2021, 2025)

panel_2001 <- panel_2001 %>%
  mutate(
    valgtype = ifelse(år %in% stortingsaar, "Storting", "Kommune"),
    valgtype = factor(valgtype, levels = c("Kommune", "Storting"))
  )




range(repr_bydel_ar_clean$ar)


colSums(is.na( <- _2001))
colSums(is.na(panel_2005))

panel_2005_2023 <- panel_2005 %>%
  filter(år <= 2023)

panel_2005_2023 <- panel_2005 %>%
  filter(år <= 2023)

panel_2005_2023 <- panel_2005_2023 %>%
  mutate(
    valgtype = ifelse(år %in% stortingsaar, "Storting", "Kommune"),
    valgtype = factor(valgtype, levels = c("Kommune", "Storting"))
  )


#Lager modellene

#en uten noen ekstra effekter, bare utd og innvandrerandel

M0 <- lm(valgdeltakelse ~ put_per_10000 +
           andel_hoyere_utd +
           innvandrerandel,
         data = panel_2001)

summary(M0)

#så en med bydelsfaste effekter

M1 <- lm(valgdeltakelse ~ put_per_10000 +
           andel_hoyere_utd +
           innvandrerandel +
           factor(geo_enhet),
         data = panel_2001)

summary(M1)

#siste med bydelsfaste effekter og tidsvariante effekter

M2 <- lm(valgdeltakelse ~ put_per_10000 +
           andel_hoyere_utd +
           innvandrerandel +
           factor(geo_enhet) +
           factor(år),
         data = panel_2001)

summary(M2)



# de samme modellene, men også med valg-dummies

M0_v <- lm(valgdeltakelse ~ put_per_10000 +
             andel_hoyere_utd +
             innvandrerandel +
             valgtype,
           data = panel_2001)

M1_v <- lm(valgdeltakelse ~ put_per_10000 +
             andel_hoyere_utd +
             innvandrerandel +
             valgtype +
             factor(geo_enhet),
           data = panel_2001)

# M2 trenger ikke valgtype (siden år-FE bør gjøre jobben)
M2_v <- M2

# Storting (PUT, NOM)


panel_2001 %>%
  group_by(geo_enhet) %>%
  summarise(sd_put = sd(put_per_10000))

cor(panel_2001$put_per_10000, as.numeric(panel_2001$år))




# gjør samme med nominelle modeller

M0_nom <- lm(valgdeltakelse ~ nom_per_10000 +
               andel_hoyere_utd +
               innvandrerandel,
             data = panel_2001)

summary(M0_nom)

M1_nom <- lm(valgdeltakelse ~ nom_per_10000 +
               andel_hoyere_utd +
               innvandrerandel +
               factor(geo_enhet),
             data = panel_2001)

summary(M1_nom)

M2_nom <- lm(valgdeltakelse ~ nom_per_10000 +
               andel_hoyere_utd +
               innvandrerandel +
               factor(geo_enhet) +
               factor(år),
             data = panel_2001)

summary(M2_nom)


# og nom-modleler med valg - samme regler som sist med M2 

M0_nom_v <- lm(valgdeltakelse ~ nom_per_10000 +
                 andel_hoyere_utd +
                 innvandrerandel +
                 valgtype,
               data = panel_2001)

M1_nom_v <- lm(valgdeltakelse ~ nom_per_10000 +
                 andel_hoyere_utd +
                 innvandrerandel +
                 valgtype +
                 factor(geo_enhet),
               data = panel_2001)

M2_nom_v <- M2_nom


# Så M0, M1 og M2-modeller med inntekt (2005-2023)
panel_2005_2023$inntekt_100k <- panel_2005_2023$inntekt / 100000

M0_put_inc <- lm(valgdeltakelse  ~ put_per_10000 +
                   andel_hoyere_utd +
                   innvandrerandel +
                   inntekt_100k,
                 data = panel_2005_2023)

M0_nom_inc <- lm(valgdeltakelse ~ nom_per_10000 +
                   andel_hoyere_utd +
                   innvandrerandel +
                   inntekt_100k,
                 data = panel_2005_2023)

M1_put_inc <- lm(valgdeltakelse ~ put_per_10000 + 
                   andel_hoyere_utd + 
                   innvandrerandel + 
                   inntekt_100k +
                   factor(geo_enhet),
                 data = panel_2005_2023)

M1_nom_inc <- lm(valgdeltakelse ~ nom_per_10000 + 
                   andel_hoyere_utd + 
                   innvandrerandel + 
                   inntekt_100k +
                   factor(geo_enhet),
                 data = panel_2005_2023)

M2_put_inc <- lm(valgdeltakelse ~ put_per_10000 +
                   andel_hoyere_utd +
                   innvandrerandel +
                   inntekt_100k +
                   factor(geo_enhet) +
                   factor(år),
                 data = panel_2005_2023)

summary(M2_put_inc)

M2_nom_inc <- lm(valgdeltakelse ~ nom_per_10000 +
                   andel_hoyere_utd +
                   innvandrerandel +
                   inntekt_100k +
                   factor(geo_enhet) +
                   factor(år),
                 data = panel_2005_2023)

summary(M2_nom_inc)






#lager norske tabeller med en funksjon, så det blir lettere å lage flere tabeller med samme funksjon
make_stargazer_no <- function(..., out_file) {
  html <- capture.output(stargazer(..., type = "html"))
  
  # faste engelske labels blir norske i stedenfor
  html <- gsub("Dependent variable:", "Avhengig variabel:", html, fixed = TRUE)
  html <- gsub("Observations", "Observasjoner", html, fixed = TRUE)
  html <- gsub("Residual Std. Error", "Residual standardfeil", html, fixed = TRUE)
  html <- gsub("F Statistic", "F-statistikk", html, fixed = TRUE)
  html <- gsub("Adjusted R\\^2", "Justert R^2", html)
  html <- gsub("Note:", "Merknad:", html, fixed = TRUE)
  
  writeLines(html, out_file)
}

# så lager jeg html-tabellene og lager dem skikkelig fineeeee

# Først PUT 2001–2025
make_stargazer_no(
  M0, M1, M2,
  title = "Effekt av representasjon på valgdeltakelse (2001–2025) – PUT+2",
  dep.var.labels = "Valgdeltakelse",
  keep = c("put_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c("PUT+2 per 10 000", "Andel høyere utdanning", "Innvandrerandel"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja")
  ),
  digits = 3,
  out_file = "tabell_put_2001_2025.html"
)


# Så tar jeg NOM 2001–2025
make_stargazer_no(
  M0_nom, M1_nom, M2_nom,
  title = "Effekt av representasjon på valgdeltakelse (2001–2025) – nominell",
  dep.var.labels = "Valgdeltakelse",
  keep = c("nom_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c("Nominelle kandidater per 10 000", "Andel høyere utdanning", "Innvandrerandel"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja")
  ),
  digits = 3,
  out_file = "tabell_nom_2001_2025.html"
)


# deretter med inntekt put
make_stargazer_no(
  M0_put_inc, M1_put_inc, M2_put_inc,
  title = "Robusthet: modeller med inntekt (2005–2023) – PUT+2",
  dep.var.labels = "Valgdeltakelse",
  keep = c("put_per_10000", "andel_hoyere_utd", "innvandrerandel", "inntekt_100k"),
  covariate.labels = c("PUT+2 per 10 000",
                       "Andel høyere utdanning",
                       "Innvandrerandel",
                       "Inntekt (per 100 000 kr)"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja")
  ),
  digits = 3,
  out_file = "tabell_put_inntekt_2005_2023.html"
)

# og en gang til med inntekt bare med  nom

make_stargazer_no(
  M0_nom_inc, M1_nom_inc, M2_nom_inc,
  title = "Robusthet: modeller med inntekt (2005–2023) – nominell",
  dep.var.labels = "Valgdeltakelse",
  keep = c("nom_per_10000", "andel_hoyere_utd", "innvandrerandel", "inntekt_100k"),
  covariate.labels = c("Nominelle kandidater per 10 000",
                       "Andel høyere utdanning",
                       "Innvandrerandel",
                       "Inntekt (per 100 000 kr)"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja")
  ),
  digits = 3,
  out_file = "tabell_nom_inntekt_2005_2023.html"
)



#kjører litt tester for å klustre SE

library(lmtest)
library(sandwich)

coeftest(M2, vcov = vcovCL(M2, cluster = ~ geo_enhet))


# liten hjelpefunksjon: henter klustrede SE som en numerisk vektor
se_cluster <- function(model, cluster_var) {
  sqrt(diag(vcovCL(model, cluster = cluster_var)))
}


se_M0  <- se_cluster(M0,  panel_2001$geo_enhet)
se_M1  <- se_cluster(M1,  panel_2001$geo_enhet)
se_M2  <- se_cluster(M2,  panel_2001$geo_enhet)

#så flere flotte tabeller

make_stargazer_no(
  M0, M1, M2,
  title = "Effekt av representasjon på valgdeltakelse (2001–2025) – PUT+2",
  dep.var.labels = "Valgdeltakelse",
  keep = c("put_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c("PUT+2 per 10 000", "Andel høyere utdanning", "Innvandrerandel"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_M0, se_M1, se_M2),
  digits = 3,
  out_file = "tabell_put_2001_2025.html"
)

se_M0n <- se_cluster(M0_nom, panel_2001$geo_enhet)
se_M1n <- se_cluster(M1_nom, panel_2001$geo_enhet)
se_M2n <- se_cluster(M2_nom, panel_2001$geo_enhet)

make_stargazer_no(
  M0_nom, M1_nom, M2_nom,
  title = "Effekt av representasjon på valgdeltakelse (2001–2025) – nominell",
  dep.var.labels = "Valgdeltakelse",
  keep = c("nom_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c("Nominelle kandidater per 10 000", "Andel høyere utdanning", "Innvandrerandel"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_M0n, se_M1n, se_M2n),
  digits = 3,
  out_file = "tabell_nom_2001_2025.html"
)

se_P0 <- se_cluster(M0_put_inc, panel_2005_2023$geo_enhet)
se_P1 <- se_cluster(M1_put_inc, panel_2005_2023$geo_enhet)
se_P2 <- se_cluster(M2_put_inc, panel_2005_2023$geo_enhet)

make_stargazer_no(
  M0_put_inc, M1_put_inc, M2_put_inc,
  title = "Robusthet: modeller med inntekt (2005–2023) – PUT+2",
  dep.var.labels = "Valgdeltakelse",
  keep = c("put_per_10000", "andel_hoyere_utd", "innvandrerandel", "inntekt_100k"),
  covariate.labels = c("PUT+2 per 10 000",
                       "Andel høyere utdanning",
                       "Innvandrerandel",
                       "Inntekt (per 100 000 kr)"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_P0, se_P1, se_P2),
  digits = 3,
  out_file = "tabell_put_inntekt_2005_2023.html"
)

se_N0 <- se_cluster(M0_nom_inc, panel_2005_2023$geo_enhet)
se_N1 <- se_cluster(M1_nom_inc, panel_2005_2023$geo_enhet)
se_N2 <- se_cluster(M2_nom_inc, panel_2005_2023$geo_enhet)

make_stargazer_no(
  M0_nom_inc, M1_nom_inc, M2_nom_inc,
  title = "Robusthet: modeller med inntekt (2005–2023) – nominell",
  dep.var.labels = "Valgdeltakelse",
  keep = c("nom_per_10000", "andel_hoyere_utd", "innvandrerandel", "inntekt_100k"),
  covariate.labels = c("Nominelle kandidater per 10 000",
                       "Andel høyere utdanning",
                       "Innvandrerandel",
                       "Inntekt (per 100 000 kr)"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_N0, se_N1, se_N2),
  digits = 3,
  out_file = "tabell_nom_inntekt_2005_2023.html"
)


summary(M2)
coeftest(M2, vcov = vcovCL(M2, cluster = ~ geo_enhet))




#mer tabell

# Klustrede SE-hjelper (har den allerede, men tar den med for helhet)


se_cluster <- function(model, cluster_vec) {
  sqrt(diag(vcovCL(model, cluster = cluster_vec)))
}

# Klustrede SE for PUT-modeller
se_M0    <- se_cluster(M0,   panel_2001$geo_enhet)
se_M0_v  <- se_cluster(M0_v, panel_2001$geo_enhet)
se_M1    <- se_cluster(M1,   panel_2001$geo_enhet)
se_M1_v  <- se_cluster(M1_v, panel_2001$geo_enhet)

make_stargazer_no(
  M0, M0_v, M1, M1_v,
  title = "Effekt av representasjon på valgdeltakelse (PUT+2): M1/M2 med og uten kontroll for valgtype",
  dep.var.labels = "Valgdeltakelse",
  keep = c("put_per_10000", "andel_hoyere_utd", "innvandrerandel", "valgtypeStorting"),
  covariate.labels = c("PUT+2 per 10 000", "Andel høyere utdanning", "Innvandrerandel", "Stortingsvalg (ref: kommune)"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Nei", "Nei"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_M0, se_M0_v, se_M1, se_M1_v),
  digits = 3,
  out_file = "tabell_put_valgtype_m0_m1.html"
)


#tilsvarende for nom

se_M0n    <- se_cluster(M0_nom,   panel_2001$geo_enhet)
se_M0n_v  <- se_cluster(M0_nom_v, panel_2001$geo_enhet)
se_M1n    <- se_cluster(M1_nom,   panel_2001$geo_enhet)
se_M1n_v  <- se_cluster(M1_nom_v, panel_2001$geo_enhet)

make_stargazer_no(
  M0_nom, M0_nom_v, M1_nom, M1_nom_v,
  title = "Effekt av representasjon på valgdeltakelse (Nominelle): M1/M2 med og uten kontroll for valgtype",
  dep.var.labels = "Valgdeltakelse",
  keep = c("nom_per_10000", "andel_hoyere_utd", "innvandrerandel", "valgtypeStorting"),
  covariate.labels = c("Nominelle kandidater per 10 000", "Andel høyere utdanning", "Innvandrerandel", "Stortingsvalg (ref: kommune)"),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Nei", "Nei"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_M0n, se_M0n_v, se_M1n, se_M1n_v),
  digits = 3,
  out_file = "tabell_nom_valgtype_m0_m1.html"
)

#lager litta figur, fint å visualisere litt. I alle fall for egen del

library(ggplot2)

# hent koeff og klustret SE for M2
b <- coef(M2)[c("put_per_10000", "andel_hoyere_utd", "innvandrerandel")]
se <- se_cluster(M2, panel_2001$geo_enhet)[c("put_per_10000", "andel_hoyere_utd", "innvandrerandel")]

df <- data.frame(
  term = c("PUT+2 per 10 000", "Andel høyere utdanning", "Innvandrerandel"),
  estimate = b,
  lo = b - 1.96 * se,
  hi = b + 1.96 * se
)

ggplot(df, aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(x = NULL, y = "Estimert koeffisient (M2, klustrede SE)",
       title = "Koeffisienter fra toveis fixed effects-modell (2001–2025)")

ggsave("figur_m2_put_koeff.png", width = 7, height = 4, dpi = 300)



#separate for kommune/storting


# Klustrede SE-hjelper
se_cluster <- function(model, cluster_var) {
  sqrt(diag(vcovCL(model, cluster = cluster_var)))
}

# Split data
panel_st <- panel_2001 %>% filter(valgtype == "Storting")
panel_ko <- panel_2001 %>% filter(valgtype == "Kommune")

# PUT: Storting
M0_put_st <- lm(valgdeltakelse ~ put_per_10000 + andel_hoyere_utd + innvandrerandel,
                data = panel_st)
M1_put_st <- lm(valgdeltakelse ~ put_per_10000 + andel_hoyere_utd + innvandrerandel + factor(geo_enhet),
                data = panel_st)

# PUT: Kommune
M0_put_ko <- lm(valgdeltakelse ~ put_per_10000 + andel_hoyere_utd + innvandrerandel,
                data = panel_ko)
M1_put_ko <- lm(valgdeltakelse ~ put_per_10000 + andel_hoyere_utd + innvandrerandel + factor(geo_enhet),
                data = panel_ko)

# Klustrede SE (klustrer på geo_enhet)
se_M0_put_st <- se_cluster(M0_put_st, panel_st$geo_enhet)
se_M1_put_st <- se_cluster(M1_put_st, panel_st$geo_enhet)
se_M0_put_ko <- se_cluster(M0_put_ko, panel_ko$geo_enhet)
se_M1_put_ko <- se_cluster(M1_put_ko, panel_ko$geo_enhet)

# Stargazer-tabell (med 4 modeller, ikke bare 3)
make_stargazer_no(
  M0_put_st, M1_put_st, M0_put_ko, M1_put_ko,
  title = "Effekt av representasjon på valgdeltakelse (PUT+2): separate modeller for stortingsvalg og kommunevalg",
  dep.var.labels = "Valgdeltakelse",
  keep = c("put_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c("PUT+2 per 10 000", "Andel høyere utdanning", "Innvandrerandel"),
  omit = c("factor\\(geo_enhet\\)"),
  add.lines = list(
    c("Utvalg", "Storting", "Storting", "Kommune", "Kommune"),
    c("Bydel FE", "Nei", "Ja", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_M0_put_st, se_M1_put_st, se_M0_put_ko, se_M1_put_ko),
  digits = 3,
  out_file = "tabell_put_valgtype_split_m0_m1.html"
)


# og også med nom

# Nom: Storting
M0_nom_st <- lm(valgdeltakelse ~ nom_per_10000 + andel_hoyere_utd + innvandrerandel,
                data = panel_st)
M1_nom_st <- lm(valgdeltakelse ~ nom_per_10000 + andel_hoyere_utd + innvandrerandel + factor(geo_enhet),
                data = panel_st)

# Nom: Kommune
M0_nom_ko <- lm(valgdeltakelse ~ nom_per_10000 + andel_hoyere_utd + innvandrerandel,
                data = panel_ko)
M1_nom_ko <- lm(valgdeltakelse ~ nom_per_10000 + andel_hoyere_utd + innvandrerandel + factor(geo_enhet),
                data = panel_ko)

# Klustrede SE
se_M0_nom_st <- se_cluster(M0_nom_st, panel_st$geo_enhet)
se_M1_nom_st <- se_cluster(M1_nom_st, panel_st$geo_enhet)
se_M0_nom_ko <- se_cluster(M0_nom_ko, panel_ko$geo_enhet)
se_M1_nom_ko <- se_cluster(M1_nom_ko, panel_ko$geo_enhet)

# Stargazer-tabell igjeeeen
make_stargazer_no(
  M0_nom_st, M1_nom_st, M0_nom_ko, M1_nom_ko,
  title = "Effekt av representasjon på valgdeltakelse (nominelle kandidater): separate modeller for stortingsvalg og kommunevalg",
  dep.var.labels = "Valgdeltakelse",
  keep = c("nom_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c("Nominelle kandidater per 10 000", "Andel høyere utdanning", "Innvandrerandel"),
  omit = c("factor\\(geo_enhet\\)"),
  add.lines = list(
    c("Utvalg", "Storting", "Storting", "Kommune", "Kommune"),
    c("Bydel FE", "Nei", "Ja", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  se = list(se_M0_nom_st, se_M1_nom_st, se_M0_nom_ko, se_M1_nom_ko),
  digits = 3,
  out_file = "tabell_nom_valgtype_split_m0_m1.html"
)

#mellom-bydels analyse

between_data <- panel_2001 %>%
  group_by(geo_enhet) %>%
  summarise(
    mean_valgdeltakelse = mean(valgdeltakelse, na.rm = TRUE),
    mean_put = mean(put_per_10000, na.rm = TRUE),
    mean_nom = mean(nom_per_10000, na.rm = TRUE),
    mean_utd = mean(andel_hoyere_utd, na.rm = TRUE),
    mean_innv = mean(innvandrerandel, na.rm = TRUE)
  )

#glemte å lage valg-dummies og alt slikt, jaja da får jeg gjøre det igjen

between_data_storting <- panel_2001 %>%
  filter(valgtype == "Storting") %>%
  group_by(geo_enhet) %>%
  summarise(
    mean_valgdeltakelse = mean(valgdeltakelse, na.rm = TRUE),
    mean_put = mean(put_per_10000, na.rm = TRUE),
    mean_nom = mean(nom_per_10000, na.rm = TRUE),
    mean_utd = mean(andel_hoyere_utd, na.rm = TRUE),
    mean_innv = mean(innvandrerandel, na.rm = TRUE),
    .groups = "drop"
  )

between_data_kommune <- panel_2001 %>%
  filter(valgtype == "Kommune") %>%
  group_by(geo_enhet) %>%
  summarise(
    mean_valgdeltakelse = mean(valgdeltakelse, na.rm = TRUE),
    mean_put = mean(put_per_10000, na.rm = TRUE),
    mean_nom = mean(nom_per_10000, na.rm = TRUE),
    mean_utd = mean(andel_hoyere_utd, na.rm = TRUE),
    mean_innv = mean(innvandrerandel, na.rm = TRUE),
    .groups = "drop"
  )

#enkel mellom-bydel regresjon. For å teste H3 (eller H4, nå husker jeg ikke helt)

between_put <- lm(mean_valgdeltakelse ~ mean_put +
                    mean_utd +
                    mean_innv,
                  data = between_data)

summary(between_put)


between_nom <- lm(mean_valgdeltakelse ~ mean_nom +
                    mean_utd +
                    mean_innv,
                  data = between_data)

summary(between_nom)

#og med valg

# Storting
between_put_st <- lm(mean_valgdeltakelse ~ mean_put + mean_utd + mean_innv,
                     data = between_data_storting)

between_nom_st <- lm(mean_valgdeltakelse ~ mean_nom + mean_utd + mean_innv,
                     data = between_data_storting)

# Kommune
between_put_ko <- lm(mean_valgdeltakelse ~ mean_put + mean_utd + mean_innv,
                     data = between_data_kommune)

between_nom_ko <- lm(mean_valgdeltakelse ~ mean_nom + mean_utd + mean_innv,
                     data = between_data_kommune)


#og uten øvrige kontroller

between_nom <- lm(mean_valgdeltakelse ~ mean_nom +
                    mean_utd +
                    mean_innv,
                  data = between_data)

summary(between_nom)

between_data_inc <- panel_2005_2023 %>%
  group_by(geo_enhet) %>%
  summarise(
    mean_valgdeltakelse = mean(valgdeltakelse, na.rm = TRUE),
    mean_put = mean(put_per_10000, na.rm = TRUE),
    mean_nom = mean(nom_per_10000, na.rm = TRUE),
    mean_utd = mean(andel_hoyere_utd, na.rm = TRUE),
    mean_innv = mean(innvandrerandel, na.rm = TRUE),
    mean_inntekt = mean(inntekt_100k, na.rm = TRUE)
  )

between_put_inc <- lm(mean_valgdeltakelse ~ mean_put +
                        mean_utd +
                        mean_innv +
                        mean_inntekt,
                      data = between_data_inc)

between_nom_inc <- lm(mean_valgdeltakelse ~ mean_nom +
                        mean_utd +
                        mean_innv +
                        mean_inntekt,
                      data = between_data_inc)

#stargazer-tabell

make_stargazer_no(
  between_put,
  between_nom,
  between_put_inc,
  between_nom_inc,
  title = "Mellom-bydel-sammenheng mellom representasjon og valgdeltakelse",
  dep.var.labels = "Gjennomsnittlig valgdeltakelse",
  keep = c("mean_put", "mean_nom", "mean_utd", "mean_innv", "mean_inntekt"),
  covariate.labels = c(
    "PUT+2 (gjennomsnitt)",
    "Nominelle kandidater (gjennomsnitt)",
    "Andel høyere utdanning (gjennomsnitt)",
    "Innvandrerandel (gjennomsnitt)",
    "Inntekt (gjennomsnitt, per 100 000 kr)"
  ),
  digits = 3,
  out_file = "tabell_mellom_bydel_full.html"
)


#tabell med valgtype

make_stargazer_no(
  between_put_st, between_nom_st, between_put_ko, between_nom_ko,
  title = "Mellom-bydel-sammenheng (separert på valgtype)",
  dep.var.labels = "Gjennomsnittlig valgdeltakelse",
  keep = c("mean_put", "mean_nom", "mean_utd", "mean_innv"),
  covariate.labels = c(
    "PUT+2 (gjennomsnitt)",
    "Nominelle kandidater (gjennomsnitt)",
    "Andel høyere utdanning (gjennomsnitt)",
    "Innvandrerandel (gjennomsnitt)"
  ),
  digits = 3,
  out_file = "tabell_mellom_bydel_valgtype.html"
)

#liten fin figur igjen

p_between <- ggplot(between_data, aes(x = mean_put, y = mean_valgdeltakelse)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Gjennomsnittlig PUT+2 per 10 000",
    y = "Gjennomsnittlig valgdeltakelse",
    title = "Mellom-bydel-sammenheng"
  ) +
  theme_minimal()

ggsave("figur_between_bydeler.png", plot = p_between,
       width = 7, height = 4, dpi = 300)


#en ny figur, mer rotete



ggplot(panel_2001, aes(x = put_per_10000, y = valgdeltakelse)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "PUT+2 per 10 000",
    y = "Valgdeltakelse",
    title = "Sammenheng mellom representasjon og valgdeltakelse (bydel-år)"
  ) +
  theme_minimal()


#figur med forskjell på valg



p_between_st <- ggplot(between_data_storting, aes(x = mean_put, y = mean_valgdeltakelse)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Gjennomsnittlig PUT+2 per 10 000 (stortingsvalg)",
    y = "Gjennomsnittlig valgdeltakelse (stortingsvalg)",
    title = "Mellom-bydel-sammenheng (storting)"
  ) +
  theme_minimal()

p_between_ko <- ggplot(between_data_kommune, aes(x = mean_put, y = mean_valgdeltakelse)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Gjennomsnittlig PUT+2 per 10 000 (kommunevalg)",
    y = "Gjennomsnittlig valgdeltakelse (kommunevalg)",
    title = "Mellom-bydel-sammenheng (kommune)"
  ) +
  theme_minimal()

p_between_ko
p_between_st

ggsave("figur_between_storting.png", plot = p_between_st, width = 7, height = 4, dpi = 300)
ggsave("figur_between_kommune.png",  plot = p_between_ko, width = 7, height = 4, dpi = 300)



#litt kjapp infot

summary(between_put_st)
summary(between_put_ko)




# lager paneler med indre-ytre, fordi det fant deskriptiv analyse at jeg måtte. De blir ganske like

panel_iy <- bydel_all_pop %>%
  filter(
    !is.na(indre_ytre),
    indre_ytre %in% valid_iy,
    !is.na(befolkning),
    befolkning > 0
  ) %>%
  group_by(ar, indre_ytre) %>%
  summarise(
    bef_region = sum(befolkning, na.rm = TRUE),
    nom_region = sum(n_kandidater_nom, na.rm = TRUE),
    put_region = sum(n_kandidater_put, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    nom_per_10000 = nom_region / bef_region * 10000,
    put_per_10000 = put_region / bef_region * 10000
  )


#iy-panel med kontroller

panel_iy_controls <- panel_2001 %>%
  left_join(
    bydel_all_pop %>% select(ar, bydel, indre_ytre, befolkning),
    by = c("år" = "ar", "geo_enhet" = "bydel")
  ) %>%
  filter(
    !is.na(indre_ytre),
    indre_ytre %in% valid_iy
  ) %>%
  group_by(år, indre_ytre) %>%
  summarise(
    valgdeltakelse =
      weighted.mean(valgdeltakelse, befolkning, na.rm = TRUE),
    
    andel_hoyere_utd =
      weighted.mean(andel_hoyere_utd, befolkning, na.rm = TRUE),
    
    innvandrerandel =
      weighted.mean(innvandrerandel, befolkning, na.rm = TRUE),
    
    .groups = "drop"
  )

#slår sammen

panel_iy <- panel_iy %>%
  left_join(panel_iy_controls,
            by = c("ar" = "år", "indre_ytre"))

#regresjonene, put og nom

M0_iy <- lm(valgdeltakelse ~ put_per_10000 +
              andel_hoyere_utd +
              innvandrerandel,
            data = panel_iy)

M1_iy <- lm(valgdeltakelse ~ put_per_10000 +
              andel_hoyere_utd +
              innvandrerandel +
              factor(indre_ytre),
            data = panel_iy)

M2_iy <- lm(valgdeltakelse ~ put_per_10000 +
              andel_hoyere_utd +
              innvandrerandel +
              factor(indre_ytre) +
              factor(ar),
            data = panel_iy)

M0_iy_nom <- lm(valgdeltakelse ~ nom_per_10000 +
                  andel_hoyere_utd +
                  innvandrerandel,
                data = panel_iy)

M1_iy_nom <- lm(valgdeltakelse ~ nom_per_10000 +
                  andel_hoyere_utd +
                  innvandrerandel +
                  factor(indre_ytre),
                data = panel_iy)

M2_iy_nom <- lm(valgdeltakelse ~ nom_per_10000 +
                  andel_hoyere_utd +
                  innvandrerandel +
                  factor(indre_ytre) +
                  factor(ar),
                data = panel_iy)


summary(panel_iy)
table(panel_iy$indre_ytre)

# Så lage tabeller


make_stargazer_no(
  M0_iy, M1_iy, M2_iy,
  title = "Effekt av representasjon på valgdeltakelse: indre–ytre panel (toppkandidater)",
  dep.var.labels = "Valgdeltakelse",
  keep = c("put_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c(
    "Toppkandidater per 10 000",
    "Andel høyere utdanning",
    "Innvandrerandel"
  ),
  omit = c("factor\\(indre_ytre\\)", "factor\\(ar\\)"),
  add.lines = list(
    c("Områdefaste effekter", "Nei", "Ja", "Ja"),
    c("Årsfaste effekter", "Nei", "Nei", "Ja"),
    c("Standardfeil", "Robuste", "Robuste", "Robuste")
  ),
  digits = 3,
  out_file = "tabell_put_indre_ytre_m0_m1_m2.html"
)


make_stargazer_no(
  M0_iy_nom, M1_iy_nom, M2_iy_nom,
  title = "Effekt av representasjon på valgdeltakelse: indre–ytre panel (nominelle kandidater)",
  dep.var.labels = "Valgdeltakelse",
  keep = c("nom_per_10000", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c(
    "Nominelle kandidater per 10 000",
    "Andel høyere utdanning",
    "Innvandrerandel"
  ),
  omit = c("factor\\(indre_ytre\\)", "factor\\(ar\\)"),
  add.lines = list(
    c("Områdefaste effekter", "Nei", "Ja", "Ja"),
    c("Årsfaste effekter", "Nei", "Nei", "Ja"),
    c("Standardfeil", "Robuste", "Robuste", "Robuste")
  ),
  digits = 3,
  out_file = "tabell_nom_indre_ytre_m0_m1_m2.html"
)




#Lager paneler med antall også:

counts_bydel <- bydel_all_pop %>%
  filter(
    !is.na(bydel),
    bydel != "Marka",
    ar >= 2001,
    ar != 2003
  ) %>%
  select(bydel, ar, n_kandidater_put, n_kandidater_nom) %>%
  rename(
    geo_enhet = bydel,
    år = ar
  )

panel_2001_ant <- panel_2001 %>%
  left_join(counts_bydel, by = c("geo_enhet", "år"))

#Så modeller på bydelsnivå, Put og Nom


M0_put_ant <- lm(valgdeltakelse ~ n_kandidater_put +
                   andel_hoyere_utd +
                   innvandrerandel,
                 data = panel_2001_ant)

M1_put_ant <- lm(valgdeltakelse ~ n_kandidater_put +
                   andel_hoyere_utd +
                   innvandrerandel +
                   factor(geo_enhet),
                 data = panel_2001_ant)

M2_put_ant <- lm(valgdeltakelse ~ n_kandidater_put +
                   andel_hoyere_utd +
                   innvandrerandel +
                   factor(geo_enhet) +
                   factor(år),
                 data = panel_2001_ant)

M0_nom_ant <- lm(valgdeltakelse ~ n_kandidater_nom +
                   andel_hoyere_utd +
                   innvandrerandel,
                 data = panel_2001_ant)

M1_nom_ant <- lm(valgdeltakelse ~ n_kandidater_nom +
                   andel_hoyere_utd +
                   innvandrerandel +
                   factor(geo_enhet),
                 data = panel_2001_ant)

M2_nom_ant <- lm(valgdeltakelse ~ n_kandidater_nom +
                   andel_hoyere_utd +
                   innvandrerandel +
                   factor(geo_enhet) +
                   factor(år),
                 data = panel_2001_ant)

#kjapp sjekk, bør være like store
nrow(panel_2001)
nrow(panel_2001_ant)

# pebe tabeller:

make_stargazer_no(
  M0_put_ant, M1_put_ant, M2_put_ant,
  title = "Effekt av representasjon på valgdeltakelse: bydelsnivå (toppkandidater, absolutte tall)",
  dep.var.labels = "Valgdeltakelse",
  keep = c("n_kandidater_put", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c(
    "Antall toppkandidater",
    "Andel høyere utdanning",
    "Innvandrerandel"
  ),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  digits = 3,
  out_file = "tabell_put_antall_bydel.html"
)


make_stargazer_no(
  M0_nom_ant, M1_nom_ant, M2_nom_ant,
  title = "Effekt av representasjon på valgdeltakelse: bydelsnivå (nominelle kandidater, absolutte tall)",
  dep.var.labels = "Valgdeltakelse",
  keep = c("n_kandidater_nom", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c(
    "Antall nominelle kandidater",
    "Andel høyere utdanning",
    "Innvandrerandel"
  ),
  omit = c("factor\\(geo_enhet\\)", "factor\\(år\\)"),
  add.lines = list(
    c("Bydel FE", "Nei", "Ja", "Ja"),
    c("År FE",    "Nei", "Nei", "Ja"),
    c("Standardfeil", "Klustret (bydel)", "Klustret (bydel)", "Klustret (bydel)")
  ),
  digits = 3,
  out_file = "tabell_nom_antall_bydel.html"
)



# samme med indre ytre nivåer

panel_iy_ant <- bydel_all_pop %>%
  filter(
    !is.na(indre_ytre),
    indre_ytre %in% valid_iy,
    !is.na(befolkning),
    befolkning > 0,
    ar >= 2001,
    ar != 2003
  ) %>%
  group_by(ar, indre_ytre) %>%
  summarise(
    n_kandidater_put = sum(n_kandidater_put, na.rm = TRUE),
    n_kandidater_nom = sum(n_kandidater_nom, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    panel_iy_controls,
    by = c("ar" = "år", "indre_ytre")
  )


# modellene, put og nom

M0_iy_put_ant <- lm(valgdeltakelse ~ n_kandidater_put +
                      andel_hoyere_utd +
                      innvandrerandel,
                    data = panel_iy_ant)

M1_iy_put_ant <- lm(valgdeltakelse ~ n_kandidater_put +
                      andel_hoyere_utd +
                      innvandrerandel +
                      factor(indre_ytre),
                    data = panel_iy_ant)

M2_iy_put_ant <- lm(valgdeltakelse ~ n_kandidater_put +
                      andel_hoyere_utd +
                      innvandrerandel +
                      factor(indre_ytre) +
                      factor(ar),
                    data = panel_iy_ant)

M0_iy_nom_ant <- lm(valgdeltakelse ~ n_kandidater_nom +
                      andel_hoyere_utd +
                      innvandrerandel,
                    data = panel_iy_ant)

M1_iy_nom_ant <- lm(valgdeltakelse ~ n_kandidater_nom +
                      andel_hoyere_utd +
                      innvandrerandel +
                      factor(indre_ytre),
                    data = panel_iy_ant)

M2_iy_nom_ant <- lm(valgdeltakelse ~ n_kandidater_nom +
                      andel_hoyere_utd +
                      innvandrerandel +
                      factor(indre_ytre) +
                      factor(ar),
                    data = panel_iy_ant)

#tabeller:

make_stargazer_no(
  M0_iy_put_ant, M1_iy_put_ant, M2_iy_put_ant,
  title = "Effekt av representasjon på valgdeltakelse: indre–ytre panel (toppkandidater, absolutte tall)",
  dep.var.labels = "Valgdeltakelse",
  keep = c("n_kandidater_put", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c(
    "Antall toppkandidater",
    "Andel høyere utdanning",
    "Innvandrerandel"
  ),
  omit = c("factor\\(indre_ytre\\)", "factor\\(ar\\)"),
  add.lines = list(
    c("Områdefaste effekter", "Nei", "Ja", "Ja"),
    c("Årsfaste effekter", "Nei", "Nei", "Ja"),
    c("Standardfeil", "Robuste", "Robuste", "Robuste")
  ),
  digits = 3,
  out_file = "tabell_put_antall_indreytre.html"
)

make_stargazer_no(
  M0_iy_nom_ant, M1_iy_nom_ant, M2_iy_nom_ant,
  title = "Effekt av representasjon på valgdeltakelse: indre–ytre panel (nominelle kandidater, absolutte tall)",
  dep.var.labels = "Valgdeltakelse",
  keep = c("n_kandidater_nom", "andel_hoyere_utd", "innvandrerandel"),
  covariate.labels = c(
    "Antall nominelle kandidater",
    "Andel høyere utdanning",
    "Innvandrerandel"
  ),
  omit = c("factor\\(indre_ytre\\)", "factor\\(ar\\)"),
  add.lines = list(
    c("Områdefaste effekter", "Nei", "Ja", "Ja"),
    c("Årsfaste effekter", "Nei", "Nei", "Ja"),
    c("Standardfeil", "Robuste", "Robuste", "Robuste")
  ),
  digits = 3,
  out_file = "tabell_nom_antall_indreytre.html"
)
