## Pakker:
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(viridis)

## Data antas å være lastet:
## bydel_all_pop <- readRDS("data/bydel_ar_repr_allpartier_pop.rds")

# Mappe for figurer
dir.create("figures", showWarnings = FALSE)

# Fargepalett (blå + oransje, ingen skikkelig grunn men hørt at det er bra mtp fargeblindhet?)
cols_type <- c(
  "Nominelle kandidater" = "#0072B2",  # blå
  "Toppkandidater"       = "#D55E00"   # oransje
)

# Former for valgtype
shapes_valg <- c(
  "Stortingsvalg"     = 16,  # sirkel
  "Kommunestyrevalg"  = 17   # trekant
)

# Funksjon: kode valgtype ut fra år (storting hvert 4. år fra 1989)
kode_valgtype <- function(ar) {
  ifelse(ar %% 4 == 1, "Stortingsvalg", "Kommunestyrevalg")
}

# Gyldige indre/ytre-kategorier
valid_iy <- c("indre øst", "indre vest", "ytre øst", "ytre vest")

# Standardisert rekkefølge på bydeler (følger teorikapittelet)
bydel_order <- c(
  "Søndre Nordstrand",
  "Østensjø",
  "Alna",
  "Bjerke",
  "Grorud",
  "Stovner",
  "Gamle Oslo",
  "Sagene",
  "Grünerløkka",
  "St. Hanshaugen",
  "Frogner",
  "Ullern",
  "Vestre Aker",
  "Nordre Aker",
  "Nordstrand"
)

# Datasett til ting som trenger befolkning
data_bydel <- bydel_all_pop %>%
  filter(
    !is.na(bydel),
    bydel %in% bydel_order,
    bydel != "Marka",
    befolkning > 0
  )

# Datasett til heatmap (uten markas befolkning)
data_bydel_heat <- bydel_all_pop %>%
  filter(
    !is.na(bydel),
    bydel %in% bydel_order,
    bydel != "Marka"
  )

# Felles ggplot-tema så blir det fint og flott og pent
theme_master <- theme_minimal(base_size = 12) +
  theme(
    legend.position      = "bottom",
    legend.title         = element_blank(),
    panel.grid.minor     = element_blank(),
    panel.grid.major.x   = element_line(colour = "grey85", linewidth = 0.3),
    panel.grid.major.y   = element_line(colour = "grey80", linewidth = 0.4)
  )


## A1: Øst-vest  antall kandidater

ov_counts <- bydel_all_pop %>%
  filter(!is.na(ost_vest)) %>%
  group_by(ar, ost_vest) %>%
  summarise(
    n_nom = sum(n_kandidater_nom, na.rm = TRUE),
    n_put = sum(n_kandidater_put, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(valgtype = kode_valgtype(ar)) %>%
  pivot_longer(
    cols      = c(n_nom, n_put),
    names_to  = "type",
    values_to = "n"
  ) %>%
  mutate(
    type = recode(type,
                  "n_nom" = "Nominelle kandidater",
                  "n_put" = "Toppkandidater")
  )

p_A1_ostvest_counts <-
  ggplot(ov_counts,
         aes(x = ar, y = n, colour = type, group = type)) +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = valgtype), size = 2.2) +
  facet_wrap(~ ost_vest) +
  scale_colour_manual(values = cols_type) +
  scale_shape_manual(values = shapes_valg) +
  labs(
    x = "År",
    y = "Antall kandidater",
    title = "Øst–vest: antall kandidater på listene",
    subtitle = "Nominelle kandidater vs toppkandidater (sirkel = stortingsvalg, trekant = kommunevalg)"
  ) +
  theme_master

p_A1_ostvest_counts

ggsave("figures/A1_ostvest_counts.png", p_A1_ostvest_counts,
       width = 9, height = 5, dpi = 300)
ggsave("figures/A1_ostvest_counts.pdf", p_A1_ostvest_counts,
       width = 9, height = 5)


## A2: Indre/ytre, antall kandidater (råtall)


iy_counts <- bydel_all_pop %>%
  filter(
    !is.na(indre_ytre),
    indre_ytre %in% valid_iy
  ) %>%
  group_by(ar, indre_ytre) %>%
  summarise(
    n_nom = sum(n_kandidater_nom, na.rm = TRUE),
    n_put = sum(n_kandidater_put, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(valgtype = kode_valgtype(ar)) %>%
  pivot_longer(
    cols      = c(n_nom, n_put),
    names_to  = "type",
    values_to = "n"
  ) %>%
  mutate(
    type = recode(type,
                  "n_nom" = "Nominelle kandidater",
                  "n_put" = "Toppkandidater")
  )

p_A2_indre_ytre_counts <-
  ggplot(iy_counts,
         aes(x = ar, y = n, colour = type, group = type)) +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = valgtype), size = 2.2) +
  facet_wrap(~ indre_ytre) +
  scale_colour_manual(values = cols_type) +
  scale_shape_manual(values = shapes_valg) +
  labs(
    x = "År",
    y = "Antall kandidater",
    title = "Indre–ytre: antall kandidater på listene",
    subtitle = "Nominelle kandidater vs toppkandidater (sirkel = stortingsvalg, trekant = kommunevalg)"
  ) +
  theme_master

p_A2_indre_ytre_counts

ggsave("figures/A2_indre_ytre_counts.png", p_A2_indre_ytre_counts,
       width = 9, height = 5, dpi = 300)
ggsave("figures/A2_indre_ytre_counts.pdf", p_A2_indre_ytre_counts,
       width = 9, height = 5)


## A3: Heatmap, bydel × år, toppkandidater (Put+2) 

# Starter i 1993, får ikke gått helt tilbake til 87 dessverre

bydel_year_put <- data_bydel_heat %>%
  filter(ar >= 1993) %>%
  group_by(ar, bydel) %>%
  summarise(
    n_put = sum(n_kandidater_put, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    bydel = factor(bydel, levels = bydel_order)
  )

p_A3_bydel_heat <-
  ggplot(bydel_year_put,
         aes(x = ar, y = bydel, fill = n_put)) +
  geom_tile() +
  scale_fill_viridis_c(
    option    = "C",
    direction = -1,
    name      = "Antall toppkandidater"
  ) +
  labs(
    x = "År",
    y = "Bydel",
    title = "Bydel × år: antall toppkandidater (Put+2)",
    subtitle = "Alle partier, 1993–>"
  ) +
  theme_master +
  theme(legend.position = "right")

p_A3_bydel_heat

ggsave("figures/A3_bydel_heat_put.png", p_A3_bydel_heat,
       width = 9, height = 6, dpi = 300)
ggsave("figures/A3_bydel_heat_put.pdf", p_A3_bydel_heat,
       width = 9, height = 6)


## B1: Øst–vest – kandidater per 10 000 innbyggere 

ov_reg <- bydel_all_pop %>%
  filter(ar >= 1995,
         !is.na(ost_vest),
         !is.na(befolkning)) %>%
  group_by(ar, ost_vest) %>%
  summarise(
    bef_region = sum(befolkning,       na.rm = TRUE),
    nom_region = sum(n_kandidater_nom, na.rm = TRUE),
    put_region = sum(n_kandidater_put, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    valgtype  = kode_valgtype(ar),
    rate_nom  = nom_region / bef_region * 10000,
    rate_put  = put_region / bef_region * 10000
  ) %>%
  pivot_longer(
    cols      = c(rate_nom, rate_put),
    names_to  = "type",
    values_to = "rate"
  ) %>%
  mutate(
    type = recode(type,
                  "rate_nom" = "Nominelle kandidater",
                  "rate_put" = "Toppkandidater")
  )

p_B1_ostvest_rate <-
  ggplot(ov_reg,
         aes(x = ar, y = rate, colour = type, group = type)) +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = valgtype), size = 2.2) +
  facet_wrap(~ ost_vest) +
  scale_colour_manual(values = cols_type) +
  scale_shape_manual(values = shapes_valg) +
  scale_y_continuous(
    breaks = seq(0, 20, 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    x = "År",
    y = "Kandidater per 10 000 innbyggere",
    title = "Øst–vest: kandidater per 10 000 innbyggere",
    subtitle = "Nominelle kandidater vs toppkandidater (sirkel = stortingsvalg, trekant = kommunevalg, 1995–>)"
  ) +
  theme_master

p_B1_ostvest_rate

ggsave("figures/B1_ostvest_rate.png", p_B1_ostvest_rate,
       width = 9, height = 5, dpi = 300)
ggsave("figures/B1_ostvest_rate.pdf", p_B1_ostvest_rate,
       width = 9, height = 5)


## B2: Indre/ytre, kandidater per 10 000 

iy_reg <- bydel_all_pop %>%
  filter(ar >= 1995,
         !is.na(indre_ytre),
         indre_ytre %in% valid_iy,
         !is.na(befolkning)) %>%
  group_by(ar, indre_ytre) %>%
  summarise(
    bef_region = sum(befolkning,       na.rm = TRUE),
    nom_region = sum(n_kandidater_nom, na.rm = TRUE),
    put_region = sum(n_kandidater_put, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  mutate(
    valgtype  = kode_valgtype(ar),
    rate_nom  = nom_region / bef_region * 10000,
    rate_put  = put_region / bef_region * 10000
  ) %>%
  pivot_longer(
    cols      = c(rate_nom, rate_put),
    names_to  = "type",
    values_to = "rate"
  ) %>%
  mutate(
    type = recode(type,
                  "rate_nom" = "Nominelle kandidater",
                  "rate_put" = "Toppkandidater")
  )

p_B2_indre_ytre_rate <-
  ggplot(iy_reg,
         aes(x = ar, y = rate, colour = type, group = type)) +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = valgtype), size = 2.2) +
  facet_wrap(~ indre_ytre) +
  scale_colour_manual(values = cols_type) +
  scale_shape_manual(values = shapes_valg) +
  scale_y_continuous(
    breaks = seq(0, 20, 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  coord_cartesian(ylim = c(0, 20)) +
  labs(
    x = "År",
    y = "Kandidater per 10 000 innbyggere",
    title = "Indre–ytre: kandidater per 10 000 innbyggere",
    subtitle = "Nominelle kandidater vs toppkandidater (sirkel = stortingsvalg, trekant = kommunevalg, 1995–>)"
  ) +
  theme_master

p_B2_indre_ytre_rate

ggsave("figures/B2_indre_ytre_rate.png", p_B2_indre_ytre_rate,
       width = 9, height = 5, dpi = 300)
ggsave("figures/B2_indre_ytre_rate.pdf", p_B2_indre_ytre_rate,
       width = 9, height = 5)


## B3: Bydeler siste år,  kandidater per 10 000, litt for gøy trolig i appendiks, men kjekt å ha som sunde ville sagt

siste_ar <- bydel_all_pop %>%
  filter(ar >= 2000, !is.na(befolkning)) %>%
  summarise(max_ar = max(ar)) %>%
  pull(max_ar)

bydel_siste <- data_bydel %>%
  filter(ar == siste_ar) %>%
  mutate(
    rate_nom = n_kandidater_nom / befolkning * 10000,
    rate_put = n_kandidater_put / befolkning * 10000,
    bydel    = factor(bydel, levels = bydel_order)
  )

bydel_siste_long <- bydel_siste %>%
  select(bydel, ost_vest, indre_ytre, rate_nom, rate_put) %>%
  pivot_longer(
    cols      = c(rate_nom, rate_put),
    names_to  = "type",
    values_to = "rate"
  ) %>%
  mutate(
    type = recode(type,
                  "rate_nom" = "Nominelle kandidater",
                  "rate_put" = "Toppkandidater")
  )

p_B3_bydel_siste <-
  ggplot(bydel_siste_long,
         aes(x = bydel, y = rate, fill = type)) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_manual(values = cols_type) +
  labs(
    x = "Bydel",
    y = "Kandidater per 10 000 innbyggere",
    title = paste0("Bydeler i ", siste_ar,
                   ": kandidater per 10 000 innbyggere"),
    subtitle = "Nominelle kandidater vs toppkandidater (alle partier)"
  ) +
  theme_master

p_B3_bydel_siste

ggsave("figures/B3_bydel_siste_rate.png", p_B3_bydel_siste,
       width = 8, height = 6, dpi = 300)
ggsave("figures/B3_bydel_siste_rate.pdf", p_B3_bydel_siste,
       width = 8, height = 6)


## B4: Øst–vest ratio m/ grå referanselinje

# By-gjennomsnitt for toppkandidater per 10 000
city_rate <- bydel_all_pop %>%
  filter(ar >= 1995, !is.na(befolkning)) %>%
  group_by(ar) %>%
  summarise(
    tot_bef   = sum(befolkning,       na.rm = TRUE),
    tot_put   = sum(n_kandidater_put, na.rm = TRUE),
    rate_city = tot_put / tot_bef * 10000,
    .groups   = "drop"
  )

# Forholdstall per region
ov_rate_top <- ov_reg %>%
  filter(type == "Toppkandidater") %>%
  select(ar, ost_vest, rate) %>%
  mutate(valgtype = kode_valgtype(ar)) %>%
  left_join(city_rate, by = "ar") %>%
  mutate(ratio = rate / rate_city)

p_B4_ratio_ostvest <-
  ggplot(ov_rate_top,
         aes(x = ar, y = ratio, colour = ost_vest, group = ost_vest)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = valgtype), size = 2.4) +
  scale_shape_manual(values = shapes_valg) +
  labs(
    x = "År",
    y = "Representasjonsratio (toppkandidater)",
    title = "Øst–vest: over- og underrepresentasjon av toppkandidater",
    subtitle = "Sirkel = stortingsvalg, trekant = kommunevalg"
  ) +
  theme_master +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0.5, 1.8, 0.25))

p_B4_ratio_ostvest

ggsave("figures/B4_ostvest_ratio_topp.png", p_B4_ratio_ostvest,
       width = 8, height = 5, dpi = 300)
ggsave("figures/B4_ostvest_ratio_topp.pdf", p_B4_ratio_ostvest,
       width = 8, height = 5)




## B5– Indre/ytre ratio med grå referanselinje (lagde først uten, det ble litt stygt)



iy_rate_top <- iy_reg %>%
  filter(type == "Toppkandidater") %>%
  select(ar, indre_ytre, rate) %>%
  mutate(valgtype = kode_valgtype(ar)) %>%
  left_join(city_rate, by = "ar") %>%
  mutate(ratio = rate / rate_city)

p_B5_ratio_indre_ytre <-
  ggplot(iy_rate_top,
         aes(x = ar, y = ratio, colour = indre_ytre, group = indre_ytre)) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.7) +
  geom_point(aes(shape = valgtype), size = 2.4) +
  scale_shape_manual(values = shapes_valg) +
  labs(
    x = "År",
    y = "Representasjonsratio (toppkandidater)",
    title = "Indre–ytre: over- og underrepresentasjon av toppkandidater",
    subtitle = "Sirkel = stortingsvalg, trekant = kommunevalg"
  ) +
  theme_master +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks = seq(0.5, 1.8, 0.25))

p_B5_ratio_indre_ytre

ggsave("figures/B5_indre_ytre_ratio_topp.png", p_B5_ratio_indre_ytre,
       width = 8, height = 5, dpi = 300)
ggsave("figures/B5_indre_ytre_ratio_topp.pdf", p_B5_ratio_indre_ytre,
       width = 8, height = 5)




valid_iy <- c("indre øst", "indre vest", "ytre øst", "ytre vest")

bydel_sone <- bydel_all_pop %>%
  filter(!is.na(bydel), bydel != "Marka", !is.na(indre_ytre), indre_ytre %in% valid_iy) %>%
  group_by(bydel) %>%
  summarise(indre_ytre = first(indre_ytre), .groups = "drop")


valg_sone <- panel_2001 %>%
  left_join(bydel_sone, by = c("geo_enhet" = "bydel")) %>%
  filter(!is.na(indre_ytre)) %>%
  group_by(år, indre_ytre) %>%
  summarise(
    valgdeltakelse_mean = mean(valgdeltakelse, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(indre_ytre = factor(indre_ytre, levels = valid_iy))

p_valg_rate <- ggplot(valg_sone, aes(x = år, y = valgdeltakelse_mean)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~ indre_ytre, ncol = 2) +
  labs(
    x = "År",
    y = "Valgdeltakelse (%)",
    title = "Valgdeltakelse over tid i fire soner",
    subtitle = "Gjennomsnitt av bydeler innen hver sone"
  ) +
  theme_minimal(base_size = 12)

p_valg_rate
ggsave("figures/valg_soner_valgdeltakelse.png", p_valg_rate, width = 9, height = 5, dpi = 300)
ggsave("figures/valg_soner_valgdeltakelse.pdf", p_valg_rate, width = 9, height = 5)





pop_bydel_ar <- bydel_all_pop %>%
  filter(!is.na(bydel), bydel != "Marka", !is.na(befolkning), befolkning > 0) %>%
  transmute(
    geo_enhet = bydel,
    år = ar,
    befolkning
  )

valg_sone_n <- panel_2001 %>%
  left_join(bydel_sone, by = c("geo_enhet" = "bydel")) %>%
  left_join(pop_bydel_ar, by = c("geo_enhet", "år")) %>%
  filter(!is.na(indre_ytre), !is.na(befolkning)) %>%
  mutate(stemmer_approx = befolkning * (valgdeltakelse / 100)) %>%
  group_by(år, indre_ytre) %>%
  summarise(
    stemmer_approx_sum = sum(stemmer_approx, na.rm = TRUE),
    befolkning_sum = sum(befolkning, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(indre_ytre = factor(indre_ytre, levels = valid_iy))

p_valg_n <- ggplot(valg_sone_n, aes(x = år, y = stemmer_approx_sum)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~ indre_ytre, ncol = 2, scales = "free_y") +
  labs(
    x = "År",
    y = "Antall stemmer (omtrent)",
    title = "Omtrent antall stemmer over tid i fire soner",
    subtitle = "Beregnet som valgdeltakelse (%) × befolkning (ikke stemmeberettigede)"
  ) +
  theme_minimal(base_size = 12)

p_valg_n
ggsave("figures/valg_soner_antall_stemmer_approx.png", p_valg_n, width = 9, height = 5, dpi = 300)
ggsave("figures/valg_soner_antall_stemmer_approx.pdf", p_valg_n, width = 9, height = 5)





## B5b: Indre vs ytre ratio men kun toppkandidater, put+2

iy2_rate_top <- iy2_reg %>%
  filter(type == "Toppkandidater") %>%
  select(ar, indre_ytre_2, rate) %>%
  mutate(valgtype = kode_valgtype(ar)) %>%
  left_join(city_rate, by = "ar") %>%
  mutate(ratio = rate / rate_city)

p_B5b_ratio_indre_ytre2 <-
  ggplot(
    iy2_rate_top,
    aes(
      x = ar,
      y = ratio,
      colour = indre_ytre_2,
      group = indre_ytre_2
    )
  ) +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_point(aes(shape = valgtype), size = 2.5) +
  scale_colour_manual(
    values = c(
      "Indre by" = "#D55E00",
      "Ytre by"  = "#0072B2"
    ),
    name = NULL
  ) +
  scale_shape_manual(
    values = shapes_valg,
    name = NULL
  ) +
  scale_y_continuous(
    breaks = seq(0.5, 1.8, 0.25),
    limits = c(0.5, 1.8)
  ) +
  labs(
    x = "År",
    y = "Representasjonsratio (toppkandidater)",
    title = "Indre–ytre: over- og underrepresentasjon av toppkandidater",
    subtitle = "Sirkel = stortingsvalg, trekant = kommunevalg"
  ) +
  theme_master +
  theme(
    legend.position = "bottom"
  )

p_B5b_ratio_indre_ytre2

ggsave("figures/B5b_indre_vs_ytre_ratio_topp.png",
       p_B5b_ratio_indre_ytre2,
       width = 8, height = 5, dpi = 300)

ggsave("figures/B5b_indre_vs_ytre_ratio_topp.pdf",
       p_B5b_ratio_indre_ytre2,
       width = 8, height = 5)
