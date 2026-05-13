# kart med parti-oversikt. Mest for egen-nysgjerrighet, kan putte i appendiks. 


library(tidyverse)
library(data.table)
library(ggplot2)

map_dt  <- data.table::copy(csmaps::oslo_ward_map_b2024_default_dt) # R klikka uten dobbel kolon, selv når jeg kjører pakken....
labs_dt <- data.table::copy(csmaps::oslo_ward_position_geolabels_b2024_default_dt)

prefix <- sub("(.*)\\d{2}$", "\\1", unique(map_dt$location_code)[1])

ward_lookup <- data.table(
  location_code = paste0(prefix, sprintf("%02d", 1:15)),
  ward_name = c(
    "Gamle Oslo","Grünerløkka","Sagene","St. Hanshaugen","Frogner",
    "Ullern","Vestre Aker","Nordre Aker","Bjerke","Grorud",
    "Stovner","Alna","Østensjø","Nordstrand","Søndre Nordstrand"
  )
)

map_dt  <- merge(map_dt,  ward_lookup, by = "location_code", all.x = TRUE)
labs_dt <- merge(labs_dt, ward_lookup, by = "location_code", all.x = TRUE)

stopifnot(!any(is.na(map_dt$ward_name)))


#fargekoder for partiene

party_colors <- c(
  "Rødt"    = "#8B0000",
  "SV"      = "#E78AC3",    #rosa er vel annerkjent SV-farge i figurer?
  "Ap"      = "#D7191C",
  "Sp"      = "#A6D96A",
  "MDG"     = "#1A9641",    #det er for mye grønt i partiene
  "Venstre" = "#1B7837",
  "KrF"     = "#FFD92F",
  "Høyre"   = "#67A9CF",
  "FrP"     = "#2166AC"
)

#leser inn data
vl_nom <- readRDS("data/valglister_golden_current.rds") #flotte datasettene mine
vl_put <- readRDS("data/valglister_golden_put.rds")


recode_party <- function(x) {
  
  x <- tolower(x)
  
  case_when(
    x %in% c("rødt", "rød valgallianse") ~ "Rødt",
    x %in% c("sosialistisk venstreparti", "sv") ~ "SV",
    x %in% c("arbeiderpartiet", "ap") ~ "Ap",
    x %in% c("senterpartiet", "sp") ~ "Sp",
    x %in% c("miljøpartiet de grønne", "mdg") ~ "MDG",
    x %in% c("venstre") ~ "Venstre",
    x %in% c("kristelig folkeparti", "krf") ~ "KrF",
    x %in% c("høyre") ~ "Høyre",
    x %in% c("fremskrittspartiet", "frp") ~ "FrP",
    TRUE ~ NA_character_
  )
}


nom_bydel <- vl_nom %>%
  mutate(parti_plot = recode_party(parti)) %>%
  filter(
    !is.na(parti_plot),
    !is.na(bydel),
    bydel != "Marka",
    ar >= 1995,
    ar <= 2025
  ) %>%
  count(parti_plot, bydel, name = "n_kandidater")


nom_bydel_complete <- expand_grid(
  parti_plot = names(party_colors),
  ward_name  = ward_lookup$ward_name
) %>%
  left_join(
    nom_bydel %>% rename(ward_name = bydel),
    by = c("parti_plot", "ward_name")
  ) %>%
  mutate(n_kandidater = replace_na(n_kandidater, 0))

map_nom_party <- map_dt %>%
  left_join(nom_bydel_complete, by = "ward_name")


nom_bydel %>%
  filter(parti_plot == "Ap") %>%
  arrange(desc(n_kandidater))

#kart for toppkandidater

put_bydel <- vl_put %>%
  mutate(parti_plot = recode_party(parti)) %>%
  filter(
    !is.na(parti_plot),
    !is.na(bydel),
    bydel != "Marka",
    ar >= 1995,
    ar <= 2025
  ) %>%
  count(parti_plot, bydel, name = "n_toppkandidater")


put_bydel_complete <- expand_grid(
  parti_plot = names(party_colors),
  ward_name  = ward_lookup$ward_name
) %>%
  left_join(
    put_bydel %>% rename(ward_name = bydel),
    by = c("parti_plot", "ward_name")
  ) %>%
  mutate(n_toppkandidater = replace_na(n_toppkandidater, 0))


map_put_party <- map_dt %>%
  left_join(put_bydel_complete, by = "ward_name")



#Funnkson for kart på parti, så slipper jeg å lage lang kode på alle parti

make_party_map <- function(map_data, party, fill_var, title, subtitle = NULL) {
  
  ggplot(
    map_data %>% filter(parti_plot == party),
    aes(x = long, y = lat, group = group, fill = .data[[fill_var]])
  ) +
    geom_polygon(colour = "grey40", linewidth = 0.2) +
    coord_quickmap() +
    scale_fill_gradient(
      low  = "grey90",
      high = party_colors[[party]],
      name = "Antall"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title      = element_text(hjust = 0, face = "bold"),
      plot.subtitle   = element_text(hjust = 0)
    )
}


#Prøver med AP

p_ap_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "Ap",
  fill_var = "n_kandidater",
  title    = "Arbeiderpartiet: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_ap_nom

p_ap_put <- make_party_map(
  map_data = map_put_party,
  party    = "Ap",
  fill_var = "n_toppkandidater",
  title    = "Arbeiderpartiet: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_ap_put

#krf

p_krf_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "KrF",
  fill_var = "n_kandidater",
  title    = "KrF: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_krf_nom

p_krf_put <- make_party_map(
  map_data = map_put_party,
  party    = "KrF",
  fill_var = "n_toppkandidater",
  title    = "KrF: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_krf_put

#Venstre

p_v_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "Venstre",
  fill_var = "n_kandidater",
  title    = "Venstre: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_v_nom

p_v_put <- make_party_map(
  map_data = map_put_party,
  party    = "Venstre",
  fill_var = "n_toppkandidater",
  title    = "Venstre: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_v_put

#Frp

p_frp_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "FrP",
  fill_var = "n_kandidater",
  title    = "FrP: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_frp_nom

p_frp_put <- make_party_map(
  map_data = map_put_party,
  party    = "FrP",
  fill_var = "n_toppkandidater",
  title    = "FrP: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_frp_put


#rødt

p_rødt_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "Rødt",
  fill_var = "n_kandidater",
  title    = "Rødt: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_rødt_nom

p_rødt_put <- make_party_map(
  map_data = map_put_party,
  party    = "Rødt",
  fill_var = "n_toppkandidater",
  title    = "Rødt: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_rødt_put


# SV

p_sv_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "SV",
  fill_var = "n_kandidater",
  title    = "SV: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_sv_nom

p_sv_put <- make_party_map(
  map_data = map_put_party,
  party    = "SV",
  fill_var = "n_toppkandidater",
  title    = "SV: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_sv_put



#MDG

p_mdg_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "MDG",
  fill_var = "n_kandidater",
  title    = "MDG: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_mdg_nom

p_mdg_put <- make_party_map(
  map_data = map_put_party,
  party    = "MDG",
  fill_var = "n_toppkandidater",
  title    = "MDG: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_mdg_put


#Høger

p_høyre_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "Høyre",
  fill_var = "n_kandidater",
  title    = "Høyre: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_høyre_nom

p_høyre_put <- make_party_map(
  map_data = map_put_party,
  party    = "Høyre",
  fill_var = "n_toppkandidater",
  title    = "Høyre: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_høyre_put


# senterpartiet


p_sp_nom <- make_party_map(
  map_data = map_nom_party,
  party    = "Sp",
  fill_var = "n_kandidater",
  title    = "Senterpartiet: kandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_sp_nom

p_sp_put <- make_party_map(
  map_data = map_put_party,
  party    = "Sp",
  fill_var = "n_toppkandidater",
  title    = "Senterpartiet: toppkandidater etter bydel",
  subtitle = "Totalt antall kandidater per bydel, 1995–2025"
)

p_sp_put









# så prøver jeg meg på ratio, selv om det blir litt mindre datamateriale



bydel_pop_avg <- bydel_all_pop %>%
  filter(
    ar >= 1995,
    ar <= 2025,
    !is.na(bydel),
    bydel != "Marka"
  ) %>%
  group_by(bydel) %>%
  summarise(
    bef_mean = mean(befolkning, na.rm = TRUE),
    .groups = "drop"
  )

#lager nom ratioer

nom_ratio <- nom_bydel %>%
  left_join(bydel_pop_avg, by = "bydel") %>%
  group_by(parti_plot) %>%
  mutate(
    andel_kand = n_kandidater / sum(n_kandidater),
    andel_bef  = bef_mean / sum(bef_mean),
    ratio      = andel_kand / andel_bef
  ) %>%
  ungroup()

nom_ratio_complete <- expand_grid(
  parti_plot = names(party_colors),
  ward_name  = ward_lookup$ward_name
) %>%
  left_join(
    nom_ratio %>% rename(ward_name = bydel),
    by = c("parti_plot", "ward_name")
  )

map_nom_ratio <- map_dt %>%
  left_join(nom_ratio_complete, by = "ward_name")


#og put/toppkandidater

put_ratio <- put_bydel %>%
  left_join(bydel_pop_avg, by = "bydel") %>%
  group_by(parti_plot) %>%
  mutate(
    andel_topp = n_toppkandidater / sum(n_toppkandidater),
    andel_bef  = bef_mean / sum(bef_mean),
    ratio      = andel_topp / andel_bef
  ) %>%
  ungroup()

put_ratio_complete <- expand_grid(
  parti_plot = names(party_colors),
  ward_name  = ward_lookup$ward_name
) %>%
  left_join(
    put_ratio %>% rename(ward_name = bydel),
    by = c("parti_plot", "ward_name")
  )


map_put_ratio <- map_dt %>%
  left_join(put_ratio_complete, by = "ward_name")



#ny kart-lagrings funksjon

make_party_ratio_map <- function(map_data, party, title, subtitle = NULL) {
  
  ggplot(
    map_data %>% filter(parti_plot == party),
    aes(x = long, y = lat, group = group, fill = ratio)
  ) +
    geom_polygon(colour = "grey40", linewidth = 0.2) +
    coord_quickmap() +
    scale_fill_gradient2(
      low      = "grey95",
      mid      = "grey75",
      high     = party_colors[[party]],
      midpoint = 1,
      name     = "Ratio"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title      = element_text(hjust = 0, face = "bold"),
      plot.subtitle   = element_text(hjust = 0)
    )
}


# og put-funksjon



make_party_ratio_map <- function(map_data, party, title, subtitle = NULL) {
  
  ggplot(
    map_data %>% filter(parti_plot == party),
    aes(x = long, y = lat, group = group, fill = ratio)
  ) +
    geom_polygon(colour = "grey40", linewidth = 0.2) +
    coord_quickmap() +
    scale_fill_gradient2(
      low      = "grey95",
      mid      = "grey75",
      high     = party_colors[[party]],
      midpoint = 1,
      name     = "Representasjons-\nratio"
    ) +
    labs(
      x = NULL,
      y = NULL,
      title = title,
      subtitle = subtitle
    ) +
    theme_void(base_size = 12) +
    theme(
      legend.position = "right",
      plot.title      = element_text(hjust = 0, face = "bold"),
      plot.subtitle   = element_text(hjust = 0)
    )
}

#prøver meg på ap

p_ap_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "Ap",
  title    = "Arbeiderpartiet: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_ap_ratio

p_ap_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "Ap",
  title    = "Arbeiderpartiet: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_ap_put_ratio


#mdg

p_mdg_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "MDG",
  title    = "MDG: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_mdg_ratio

p_mdg_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "MDG",
  title    = "MDG: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_mdg_put_ratio

# SV

p_sv_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "SV",
  title    = "SV: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_sv_ratio

p_sv_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "SV",
  title    = "SV: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_sv_put_ratio


#Raudt

p_rødt_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "Rødt",
  title    = "Rødt: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_rødt_ratio

p_rødt_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "Rødt",
  title    = "Rødt: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_rødt_put_ratio


#SP

p_sp_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "Sp",
  title    = "Senterpartiet: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_sp_ratio

p_sp_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "Sp",
  title    = "Senterpartiet: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_sp_put_ratio


# venstre

p_v_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "Venstre",
  title    = "Venstre: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_v_ratio

p_v_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "Venstre",
  title    = "Venstre: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_v_put_ratio


# krf

p_krf_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "KrF",
  title    = "KrF: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_krf_ratio

#helt enoooormt nordstrand/søndre nordstrand her
p_krf_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "KrF",
  title    = "KrF: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_krf_put_ratio


# høger

p_høyre_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "Høyre",
  title    = "Høyre: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_høyre_ratio

p_høyre_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "Høyre",
  title    = "Høyre: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_høyre_put_ratio


#Framstegspartiet

p_frp_ratio <- make_party_ratio_map(
  map_data = map_nom_ratio,
  party    = "FrP",
  title    = "FrP: representasjon etter bydel",
  subtitle = "Ratio mellom kandidatandel og befolkningsandel, 1995–2025"
)

p_frp_ratio

p_frp_put_ratio <- make_party_ratio_map(
  map_data = map_put_ratio,
  party    = "FrP",
  title    = "FrP: toppkandidater etter bydel",
  subtitle = "Ratio mellom toppkandidatandel og befolkningsandel, 1995–2025"
)

p_frp_put_ratio




#lager grids

library(patchwork)

#SV

p_sv_grid <- (p_sv_nom | p_sv_put) / (p_sv_ratio | p_sv_put_ratio) +
  plot_annotation(
    title = "SV: geografisk representasjon etter bydel",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
    )
  )

p_sv_grid

ggsave(
  filename = "figures/p_sv_grid.png",
  plot = p_sv_grid,
  width = 12,
  height = 10,
  dpi = 300
)

# så resten, lager loop for ta alle i en go


party_ids <- c("rødt", "sv", "ap", "sp", "mdg", "v", "krf", "høyre", "frp")
party_labels <- c(
  "Rødt", "SV", "Arbeiderpartiet", "Senterpartiet", "MDG",
  "Venstre", "KrF", "Høyre", "Fremskrittspartiet"
)

for (i in seq_along(party_ids)) {
  
  pid <- party_ids[i]
  plab <- party_labels[i]
  
  g <- (get(paste0("p_", pid, "_nom")) | get(paste0("p_", pid, "_put"))) /
    (get(paste0("p_", pid, "_ratio")) | get(paste0("p_", pid, "_put_ratio"))) +
    plot_annotation(
      title = paste0(plab, ": geografisk representasjon etter bydel"),
      theme = theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
      )
    )
  
  ggsave(
    filename = paste0("figures/p_", pid, "_grid.png"),
    plot = g,
    width = 12,
    height = 10,
    dpi = 300
  )
}


