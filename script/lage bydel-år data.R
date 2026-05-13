library(dplyr)
library(stringr)

vl_nom <- readRDS("data/valglister_golden_current.rds")
vl_put <- readRDS("data/valglister_golden_put.rds")

# Sjekk at bydel og ar finnes:
# names(vl_nom)

# NOMINELL: antall kandidater per bydel–år (alle partier)
bydel_nom_all <- vl_nom %>%
  group_by(ar, bydel, indre_ytre, ost_vest) %>%
  summarise(
    n_kandidater_nom = n(),
    .groups = "drop"
  )

# TOPPKANDIDATER (Put+2): antall per bydel–år (alle partier)
bydel_put_all <- vl_put %>%
  group_by(ar, bydel, indre_ytre, ost_vest) %>%
  summarise(
    n_kandidater_put = n(),
    .groups = "drop"
  )

# Slå sammen til ett bydel–år-datasett
bydel_all <- bydel_nom_all %>%
  full_join(
    bydel_put_all,
    by = c("ar", "bydel", "indre_ytre", "ost_vest")
  ) %>%
  mutate(
    n_kandidater_nom = ifelse(is.na(n_kandidater_nom), 0L, n_kandidater_nom),
    n_kandidater_put = ifelse(is.na(n_kandidater_put), 0L, n_kandidater_put)
  )

#saveRDS(bydel_all, "data/bydel_ar_repr_allpartier.rds")







store_kjerne <- c(
  "arbeiderpartiet",
  "høyre",
  "fremskrittspartiet",
  "kristelig folkeparti",
  "venstre",
  "senterpartiet",
  "sosialistisk venstreparti"
)

vl_nom_store <- vl_nom %>%
  filter(
    parti %in% store_kjerne |
      (parti == "rød valgallianse" &
         dplyr::between(ar, 1987, 2007)) |
      (parti %in% c("rødt", "miljøpartiet de grønne") &
         ar >= 2009)
  )

vl_put_store <- vl_put %>%
  filter(
    parti %in% store_kjerne |
      (parti == "rød valgallianse" &
         dplyr::between(ar, 1987, 2007)) |
      (parti %in% c("rødt", "miljøpartiet de grønne") &
         ar >= 2009)
  )

bydel_nom_store <- vl_nom_store %>%
  group_by(ar, bydel, indre_ytre, ost_vest) %>%
  summarise(
    n_kandidater_nom = n(),
    .groups = "drop"
  )

bydel_put_store <- vl_put_store %>%
  group_by(ar, bydel, indre_ytre, ost_vest) %>%
  summarise(
    n_kandidater_put = n(),
    .groups = "drop"
  )

bydel_store <- bydel_nom_store %>%
  full_join(
    bydel_put_store,
    by = c("ar", "bydel", "indre_ytre", "ost_vest")
  ) %>%
  mutate(
    n_kandidater_nom = ifelse(is.na(n_kandidater_nom), 0L, n_kandidater_nom),
    n_kandidater_put = ifelse(is.na(n_kandidater_put), 0L, n_kandidater_put)
  )

#saveRDS(bydel_store, "data/bydel_ar_repr_storepartier.rds")









library(tidyr)

pop_raw <- read.csv("Befolkningsutvikling  i Oslo  (1).csv",
                    sep = ";", check.names = FALSE)

pop1 <- pop_raw %>%
  rename(bydel_raw = 1)

pop_tidy <- pop1 %>%
  pivot_longer(
    cols      = -bydel_raw,
    names_to  = "kolonne",
    values_to = "befolkning"
  ) %>%
  mutate(
    ar = stringr::str_extract(kolonne, "\\d{4}"),
    ar = as.integer(ar)
  ) %>%
  filter(!is.na(ar)) %>%
  select(bydel_raw, ar, befolkning) %>%
  mutate(
    bydel = bydel_raw %>%
      stringr::str_replace("^Bydel ", "") %>%
      stringr::str_squish() %>%
      stringr::str_to_lower()
  )

# Bare valgår vi faktisk har representasjonsdata for (og fra 2000)
bydel_all <- readRDS("data/bydel_ar_repr_allpartier.rds")
valgar <- sort(unique(bydel_all$ar))

pop_valg <- pop_tidy %>%
  filter(ar %in% valgar,
         ar >= 2000)

#saveRDS(pop_valg, "data/befolkning_bydel_valgar_2000_2025.rds")




view(pop_valg)

#Slå sammen
library(dplyr)
library(stringr)

bydel_all <- readRDS("data/bydel_ar_repr_allpartier.rds")
pop_valg  <- readRDS("data/befolkning_bydel_valgar_2000_2025.rds")

bydel_all_pop <- bydel_all %>%
  mutate(
    bydel_key = bydel %>%
      str_squish() %>%
      str_to_lower()
  ) %>%
  left_join(
    pop_valg %>%
      mutate(
        bydel_key = bydel %>%
          str_squish() %>%
          str_to_lower()
      ) %>%
      select(bydel_key, ar, befolkning),
    by = c("bydel_key", "ar")
  ) %>%
  select(-bydel_key)  # rydder vekk nøkkelen hvis du ikke vil ha den med

#saveRDS(bydel_all_pop, "data/bydel_ar_repr_allpartier_pop.rds")





#Slå sammen store


bydel_store <- readRDS("data/bydel_ar_repr_storepartier.rds")

bydel_store_pop <- bydel_store %>%
  mutate(
    bydel_key = bydel %>%
      str_squish() %>%
      str_to_lower()
  ) %>%
  left_join(
    pop_valg %>%
      mutate(
        bydel_key = bydel %>%
          str_squish() %>%
          str_to_lower()
      ) %>%
      select(bydel_key, ar, befolkning),
    by = c("bydel_key", "ar")
  ) %>%
  select(-bydel_key)

#saveRDS(bydel_store_pop, "data/bydel_ar_repr_storepartier_pop.rds")



bydel_store_pop


bydel_all_pop %>% filter(ar >= 2000) %>% select(bydel, ar, befolkning) %>% head()
bydel_all_pop %>% summarise(andelen_med_bef = mean(!is.na(befolkning)))
