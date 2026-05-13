
# Kart for representasjon: ratio på bydels- og indre/ytre-nivå
# laster pakker som trengs for dette scripet

library(csmaps)
library(data.table)
library(dplyr)
library(ggplot2)
library(scales)

# setter sti der jeg vil lagre karta mine

#dir.create("figures", showWarnings = FALSE)

# 1. Oslo-kart:ordner bydelspolygoner og etiketter, henter samme som kart til teorikapittel

map_dt  <- data.table::copy(csmaps::oslo_ward_map_b2024_default_dt)
labs_dt <- data.table::copy(csmaps::oslo_ward_position_geolabels_b2024_default_dt)

# 1) Finner faktisk prefiks før de to siste sifrene (01–15)
prefix <- sub("(.*)\\d{2}$", "\\1", unique(map_dt$location_code)[1])

# 2) Lager lookup med riktige koder og navn
ward_lookup <- data.table(
  location_code = paste0(prefix, sprintf("%02d", 1:15)),
  ward_name = c(
    "Gamle Oslo","Grünerløkka","Sagene","St. Hanshaugen","Frogner",
    "Ullern","Vestre Aker","Nordre Aker","Bjerke","Grorud",
    "Stovner","Alna","Østensjø","Nordstrand","Søndre Nordstrand"
  )
)

# 3) Join inn ward_name på kart og etiketter
map_dt  <- merge(map_dt,  ward_lookup, by = "location_code", all.x = TRUE)
labs_dt <- merge(labs_dt, ward_lookup, by = "location_code", all.x = TRUE)

stopifnot(!any(is.na(map_dt$ward_name)))

bydel_all_pop <- readRDS("data/bydel_ar_repr_allpartier_pop.rds")

# 2. Beregner representasjonsratio per bydel - dette for å kunne se per innbygger

# Renset versjon for ratio-beregninger
data_bydel_ratio <- bydel_all_pop %>%
  filter(
    !is.na(bydel),
    bydel != "Marka",
    !is.na(befolkning),
    befolkning > 0
  )

bydel_ratio <- data_bydel_ratio %>%
  group_by(bydel) %>%
  summarise(
    topp_tot = sum(n_kandidater_put, na.rm = TRUE),
    nom_tot  = sum(n_kandidater_nom, na.rm = TRUE),
    bef_tot  = sum(befolkning,       na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    andel_topp = topp_tot / sum(topp_tot),
    andel_nom  = nom_tot  / sum(nom_tot),
    andel_bef  = bef_tot  / sum(bef_tot),
    ratio_topp = andel_topp / andel_bef,
    ratio_nom  = andel_nom  / andel_bef
  )

# Lang-versjon for facet (nom vs topp)
bydel_ratio_long <- bydel_ratio %>%
  select(bydel, ratio_nom, ratio_topp) %>%
  tidyr::pivot_longer(
    cols      = c(ratio_nom, ratio_topp),
    names_to  = "type",
    values_to = "ratio"
  ) %>%
  mutate(
    type = recode(type,
                  "ratio_nom"  = "Nominelle kandidater",
                  "ratio_topp" = "Toppkandidater (Put+2)")
  )

# 3. Kart ratio toppkandidater per bydel 


map_ratio <- map_dt %>%
  left_join(bydel_ratio %>% rename(ward_name = bydel), by = "ward_name")

p_map_bydel_topp <-
  ggplot(map_ratio,
         aes(x = long, y = lat, group = group, fill = ratio_topp)) +
  geom_polygon(colour = "grey40", linewidth = 0.2) +
  coord_quickmap() +
  scale_fill_gradient2(
    low      = "#2166AC",   # blå = underrepresentert
    mid      = "white",     # Hvit = "riktig" representert
    high     = "#B2182B",   # rød = overrepresentert
    midpoint = 1,
    name     = "Ratio\n(toppkandidater)"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Over- og underrepresentasjon av toppkandidater (Put+2)",
    subtitle = "Ratio > 1: flere toppkandidater enn befolkningsandel tilsier"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title      = element_text(hjust = 0, face = "bold"),
    plot.subtitle   = element_text(hjust = 0)
  )

p_map_bydel_topp

ggsave("figures/C1_bydel_ratio_topp.png", p_map_bydel_topp,
       width = 6, height = 6, dpi = 300)
ggsave("figures/C1_bydel_ratio_topp.pdf", p_map_bydel_topp,
       width = 6, height = 6)

# 4. Kart facet: nominell vs toppkandidater (put)

map_ratio_long <- map_dt %>%
  left_join(bydel_ratio_long %>% rename(ward_name = bydel), by = "ward_name")

p_map_bydel_facets <-
  ggplot(map_ratio_long,
         aes(x = long, y = lat, group = group, fill = ratio)) +
  geom_polygon(colour = "grey40", linewidth = 0.2) +
  coord_quickmap() +
  facet_wrap(~ type) +
  scale_fill_gradient2(
    low      = "#2166AC",
    mid      = "white",
    high     = "#B2182B",
    midpoint = 1,
    name     = "Ratio"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Over- og underrepresentasjon per bydel",
    subtitle = "Sammenlikning av nominelle kandidater og toppkandidater (Put+2), 1987–2025"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text      = element_text(face = "bold"),
    plot.title      = element_text(hjust = 0, face = "bold"),
    plot.subtitle   = element_text(hjust = 0)
  )

p_map_bydel_facets

#automatisk lagring

ggsave("figures/C2_bydel_ratio_nom_vs_topp.png", p_map_bydel_facets,
       width = 9, height = 5.5, dpi = 300)
ggsave("figures/C2_bydel_ratio_nom_vs_topp.pdf", p_map_bydel_facets,
       width = 9, height = 5.5)

# 5. Kart C4: indre/ytre – ratio toppkandidater, fire paneler
#    (alle bydeler i samme område får samme verdi/farge)


valid_iy <- c("indre øst", "indre vest", "ytre øst", "ytre vest")

# 1. Ratio per bydel (nom + topp)

data_bydel_ratio <- bydel_all_pop %>%
  filter(
    !is.na(bydel),
    bydel != "Marka",
    !is.na(befolkning),
    befolkning > 0
  )

bydel_ratio <- data_bydel_ratio %>%
  group_by(bydel) %>%
  summarise(
    topp_tot = sum(n_kandidater_put, na.rm = TRUE),
    nom_tot  = sum(n_kandidater_nom, na.rm = TRUE),
    bef_tot  = sum(befolkning,       na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    andel_topp = topp_tot / sum(topp_tot),
    andel_nom  = nom_tot  / sum(nom_tot),
    andel_bef  = bef_tot  / sum(bef_tot),
    ratio_topp = andel_topp / andel_bef,
    ratio_nom  = andel_nom  / andel_bef
  )

# 2. Ratio per indre/ytre-område (nom + topp) 

iy_ratio_both <- bydel_all_pop %>%
  filter(
    !is.na(indre_ytre),
    indre_ytre %in% valid_iy,
    !is.na(befolkning),
    befolkning > 0
  ) %>%
  group_by(indre_ytre) %>%
  summarise(
    topp_tot = sum(n_kandidater_put, na.rm = TRUE),
    nom_tot  = sum(n_kandidater_nom, na.rm = TRUE),
    bef_tot  = sum(befolkning,       na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    andel_topp = topp_tot / sum(topp_tot),
    andel_nom  = nom_tot  / sum(nom_tot),
    andel_bef  = bef_tot  / sum(bef_tot),
    ratio_topp = andel_topp / andel_bef,
    ratio_nom  = andel_nom  / andel_bef
  )

# 3. Kobler bydelene til indre/ytre-koder 

bydel_indre_ytre <- bydel_all_pop %>%
  filter(
    !is.na(bydel),
    bydel != "Marka",
    !is.na(indre_ytre),
    indre_ytre %in% valid_iy
  ) %>%
  group_by(bydel) %>%
  summarise(
    indre_ytre = dplyr::first(indre_ytre),
    .groups    = "drop"
  )

# 4. Lager fire datasett som alle har hele Oslo-kartet, men ulike "panel"-etiketter og ulike ratio-verdier 

# (1) Bydel de nominelle
map_bydel_nom <- map_dt %>%
  left_join(bydel_ratio %>% rename(ward_name = bydel), by = "ward_name") %>%
  mutate(
    panel = "Bydel – nominelle kandidater",
    value = ratio_nom
  )

# (2) Bydel kun toppkandidater
map_bydel_topp <- map_dt %>%
  left_join(bydel_ratio %>% rename(ward_name = bydel), by = "ward_name") %>%
  mutate(
    panel = "Bydel – toppkandidater (Put+2)",
    value = ratio_topp
  )

# (3) Indre/ytre de nominelle
map_iy_nom <- map_dt %>%
  left_join(bydel_indre_ytre %>% rename(ward_name = bydel), by = "ward_name") %>%
  left_join(iy_ratio_both %>% select(indre_ytre, ratio_nom), by = "indre_ytre") %>%
  mutate(
    panel = "Indre/ytre – nominelle kandidater",
    value = ratio_nom
  )

# (4) Indre/ytre kun toppkandidater
map_iy_topp <- map_dt %>%
  left_join(bydel_indre_ytre %>% rename(ward_name = bydel), by = "ward_name") %>%
  left_join(iy_ratio_both %>% select(indre_ytre, ratio_topp), by = "indre_ytre") %>%
  mutate(
    panel = "Indre/ytre – toppkandidater (Put+2)",
    value = ratio_topp
  )

# 5. Slå sammen alle til ett stort datasett 

map_all_panels <- bind_rows(
  map_bydel_nom,
  map_bydel_topp,
  map_iy_nom,
  map_iy_topp
) %>%
  mutate(
    panel = factor(
      panel,
      levels = c(
        "Bydel – nominelle kandidater",
        "Bydel – toppkandidater (Put+2)",
        "Indre/ytre – nominelle kandidater",
        "Indre/ytre – toppkandidater (Put+2)"
      )
    )
  )

# 6. Plot: 2×2-panel med felles skala 
# Jeg elsker paneler

p_C4_four_panel <-
  ggplot(map_all_panels,
         aes(x = long, y = lat, group = group, fill = value)) +
  geom_polygon(colour = "grey40", linewidth = 0.2) +
  coord_quickmap() +
  facet_wrap(~ panel, ncol = 2) +
  scale_fill_gradient2(
    low      = "#2166AC",   # blå = underrepresentert
    mid      = "white",
    high     = "#B2182B",   # rød = overrepresentert
    midpoint = 1,
    name     = "Ratio"
  ) +
  labs(
    x = NULL, y = NULL,
    title = "Geografisk fordeling av representasjon i Oslo",
    subtitle = "Ratio > 1: flere kandidater enn befolkningsandel tilsier"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    strip.text      = element_text(face = "bold"),
    plot.title      = element_text(hjust = 0, face = "bold"),
    plot.subtitle   = element_text(hjust = 0)
  )

p_C4_four_panel

ggsave("figures/C4_fire_panel_ratio.png", p_C4_four_panel,
       width = 10, height = 7, dpi = 300)
ggsave("figures/C4_fire_panel_ratio.pdf", p_C4_four_panel,
       width = 10, height = 7)








#andre figurer

# 1. Beregn totale nominelle kandidater per bydel

data_bydel_nom <- bydel_all_pop %>%
  filter(
    !is.na(bydel),
    bydel != "Marka"
  )

bydel_nom_tot <- data_bydel_nom %>%
  group_by(bydel) %>%
  summarise(
    nom_tot = sum(n_kandidater_nom, na.rm = TRUE),
    .groups = "drop"
  )


# 2. Barplot: totale nominelle kandidater per bydel 

# Rekkefølge: mest nominelle øverst
bydel_order_nom <- bydel_nom_tot %>%
  arrange(desc(nom_tot)) %>%
  pull(bydel)

bydel_nom_tot <- bydel_nom_tot %>%
  mutate(bydel = factor(bydel, levels = bydel_order_nom))

p_nom_bar <-
  ggplot(bydel_nom_tot,
         aes(x = bydel, y = nom_tot)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(
    x = "Bydel",
    y = "Antall nominelle kandidater (totalt, 1987–2025)",
    title = "Nominell geografisk fordeling av kandidater",
    subtitle = "Totalt antall kandidater per bydel gjennom hele perioden"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

p_nom_bar

ggsave("figures/N1_bydel_nom_tot_bar.png", p_nom_bar,
       width = 8, height = 6, dpi = 300)
ggsave("figures/N1_bydel_nom_tot_bar.pdf", p_nom_bar,
       width = 8, height = 6)

## 3. Kart: nominelle kandidater per bydel 


map_nom <- map_dt %>%
  left_join(bydel_nom_tot %>% rename(ward_name = bydel),
            by = "ward_name")

# Sjekk for eventuelle NA i tilfelle wacke resultater
# summary(map_nom$nom_tot)

p_nom_map <-
  ggplot(map_nom,
         aes(x = long, y = lat, group = group, fill = nom_tot)) +
  geom_polygon(colour = "grey40", linewidth = 0.2) +
  coord_quickmap() +
  scale_fill_viridis_c(
    option = "C",
    name   = "Antall\nkandidater"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Nominell fordeling av kandidater på bydelsnivå",
    subtitle = "Totalt antall kandidater per bydel, 1987–2025"
  ) +
  theme_void(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title      = element_text(hjust = 0, face = "bold"),
    plot.subtitle   = element_text(hjust = 0)
  )

p_nom_map

ggsave("figures/N2_bydel_nom_tot_map.png", p_nom_map,
       width = 6, height = 6, dpi = 300)
ggsave("figures/N2_bydel_nom_tot_map.pdf", p_nom_map,
       width = 6, height = 6)





