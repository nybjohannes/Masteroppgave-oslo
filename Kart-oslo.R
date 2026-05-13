library(csmaps)
library(data.table)
library(ggplot2)
library(patchwork)
library(grid)

map_dt  <- copy(csmaps::oslo_ward_map_b2024_default_dt)
labs_dt <- copy(csmaps::oslo_ward_position_geolabels_b2024_default_dt)

# 1) Finn faktisk prefiks før de to siste sifrene (01–15)
prefix <- sub("(.*)\\d{2}$", "\\1", unique(map_dt$location_code)[1])

# 2) Lag lookup med riktige koder
ward_lookup <- data.table(
  location_code = paste0(prefix, sprintf("%02d", 1:15)),
  ward_name = c(
    "Gamle Oslo","Grünerløkka","Sagene","St. Hanshaugen","Frogner",
    "Ullern","Vestre Aker","Nordre Aker","Bjerke","Grorud",
    "Stovner","Alna","Østensjø","Nordstrand","Søndre Nordstrand"
  )
)

# 3) Left join: legg til ward_name på polygoner og etikettpunkter
map_dt  <- merge(map_dt,  ward_lookup, by = "location_code", all.x = TRUE)
labs_dt <- merge(labs_dt, ward_lookup, by = "location_code", all.x = TRUE)

# 4) verifiserer at det funker
stopifnot(!any(is.na(map_dt$ward_name)))


# Helt clean kart over Oslo med bydeler og navn
oslo_bydelskart <- ggplot(mapping = aes(x = long, y = lat)) +
  geom_polygon(
    data = map_dt,
    aes(group = group),
    fill = "grey92",
    color = "grey35",
    linewidth = 0.35
  ) +
  geom_label(
    data = labs_dt,
    aes(label = ward_name),
    size = 3,
    color = "grey15",
    fill = scales::alpha("white", 0.85),
    label.size = 0,
    label.padding = unit(1.1, "mm")
  ) +
  coord_quickmap() +
  theme_void() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5)
  )

oslo_bydelskart

ggsave("oslo_bydelskart.png", oslo_bydelskart, width = 8, height = 8, dpi = 300)


#nye kart med inndelinger 

#Først fargepalett som samsvarer med word-tabellen i oppgaven
# To-nivå (Øst/vest) 
pal_kant2 <- c(
  "Vestkant" = "#9BC6E9",  # lys blå
  "Østkant"  = "#F2A7C4"   # lys rosa
)

#  Fire-nivå (Indre/ytre × Øst/vest) 
pal_kant4 <- c(
  "Indre vestkant" = "#7FB3E6",  # litt mørkere blå
  "Ytre vestkant"  = "#CDE6F6",  # lysere blå
  "Indre østkant"  = "#E59AC7",  # litt mørkere rosa
  "Ytre østkant"   = "#F4C6DF"   # lysere rosa
)




#så lager jeg selve kartet:

# 5) Plot
ggplot(mapping = aes(long, lat)) +
  geom_polygon(
    data = map_dt,
    mapping = aes(group = group, fill = ward_name),
    color = "black", linewidth = 0.4
  ) +
  geom_label(
    data = labs_dt,
    mapping = aes(label = ward_name),
    color = "red"
  ) +
  guides(fill = guide_legend(title = "Bydel")) +
  theme_void() +
  coord_quickmap()


vestkant <- c("Frogner","St. Hanshaugen","Ullern","Vestre Aker","Nordre Aker","Nordstrand")

østkant <- setdiff(ward_lookup$ward_name, vestkant)


class_tab <- data.table(
  ward_name = ward_lookup$ward_name
)[
  , kant2 := fifelse(ward_name %in% vestkant, "Vestkant", "Østkant")
]



# så med indre og ytre:


# INDRE / YTRE × ØST / VEST
indre_vest <- c("Frogner","St. Hanshaugen")
ytre_vest  <- c("Ullern","Vestre Aker","Nordre Aker","Nordstrand")         # ← din override
indre_øst  <- c("Gamle Oslo","Grünerløkka","Sagene")
ytre_øst   <- c("Bjerke","Grorud","Stovner","Alna","Østensjø","Søndre Nordstrand")

class_tab[
  ward_name %in% indre_vest, kant4 := "Indre vestkant"
][
  ward_name %in% ytre_vest,  kant4 := "Ytre vestkant"
][
  ward_name %in% indre_øst,  kant4 := "Indre østkant"
][
  ward_name %in% ytre_øst,   kant4 := "Ytre østkant"
]

# legg inn klassene på kart- og label-data
map_dt  <- merge(map_dt,  class_tab, by = "ward_name", all.x = TRUE)
labs_dt <- merge(labs_dt, class_tab, by = "ward_name", all.x = TRUE)

stopifnot(!any(is.na(map_dt$kant2)), !any(is.na(map_dt$kant4)))





# Pynt som kan gjenbrukes på begge kart:

panelramme <- theme(panel.border = element_rect(color = "black", fill = NA, linewidth = 0.6))

# To-nivå-kart:
kart_2 <- ggplot(mapping = aes(x = long, y = lat)) +
  geom_polygon(
    data = map_dt,
    aes(group = group, fill = factor(kant2, levels = c("Vestkant","Østkant"))),
    color = "grey20", linewidth = 0.35
  ) +
  geom_label(
    data = labs_dt,
    aes(label = ward_name),
    size = 3, color = "grey20",
    label.size = 0, fill = scales::alpha("white", 0.75),
    label.padding = unit(1.2, "mm")
  ) +
  scale_fill_manual(values = pal_kant2, drop = FALSE, name = "Kant") +
  coord_quickmap() +
  theme_void() +
  theme(
    legend.position = c(0.10, 0.15),
    legend.justification = c(0, 0)
  ) +
  panelramme

# Nivå-rekkefølge for kant4:
map_dt$kant4  <- factor(map_dt$kant4,
                        levels = c("Indre vestkant","Ytre vestkant","Indre østkant","Ytre østkant"))
labs_dt$kant4 <- factor(labs_dt$kant4, levels = levels(map_dt$kant4))

# Tre-nivå-kart (din kode, lett ryddet)
kart_3 <- ggplot(mapping = aes(x = long, y = lat)) +
  geom_polygon(
    data = map_dt,
    aes(group = group, fill = kant4),
    color = "grey20", linewidth = 0.35
  ) +
  geom_label(
    data = labs_dt,
    aes(label = ward_name),
    size = 3, color = "grey20",
    label.size = 0, fill = scales::alpha("white", 0.75),
    label.padding = unit(1.2, "mm")
  ) +
  scale_fill_manual(values = pal_kant4, drop = FALSE, name = "Inndeling") +
  coord_quickmap() +
  theme_void() +
  theme(
    legend.position = c(0.10, 0.15),
    legend.justification = c(0, 0)
  ) +
  panelramme

# --- Grid med panel-tagger (a) og (b) ---
figur_grid <- figur_grid <- patchwork::wrap_plots(kart_2, kart_3, ncol = 2) +
  patchwork::plot_annotation(tag_levels = "a", tag_prefix = "(", tag_suffix = ")")

ggsave("figur_2_grid.png", figur_grid, width = 14, height = 7.5, dpi = 300)


# Lagre (bruk PNG til Word; PDF om du lager PDF-rapport)
ggsave("figur_2_grid.png", figur_grid, width = 14, height = 7.5, dpi = 300)
# ggsave("figur_2_grid.pdf", figur_grid, width = 14, height = 7.5)




install.packages("PxWebApiData")

install.packages("readstata13")




