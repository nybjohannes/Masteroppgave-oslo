setwd("/Users/johannesnyborg/Documents/Master/Masteroppgave/Data/Deskriptiv Analyse Oslo")
bydel_all_pop <- readRDS("data_intermediate/bydel_ar_repr_allpartier_pop.rds")


file.exists("data_intermediate/bydel_ar_repr_allpartier_pop.rds")




bydel_all_pop <- readRDS("data_intermediate/bydel_ar_repr_allpartier_pop.rds")

# sanity
range(bydel_all_pop$ar, na.rm = TRUE)
sum(is.na(bydel_all_pop$befolkning))


file.copy(
  "data_intermediate/bydel_ar_repr_allpartier_pop.rds",
  "data_intermediate/bydel_ar_repr_allpartier_pop_BACKUP.rds",
  overwrite = TRUE
)


library(readxl)
library(dplyr)

innb_patch <- read_excel("befolkning.xlsx") %>%
  transmute(
    bydel = trimws(bydel),
    ar = as.integer(ar),
    befolkning_patch = as.numeric(befolkning)
  ) %>%
  filter(ar %in% c(1995, 1997, 1999))


bydel_all_pop <- bydel_all_pop %>%
  left_join(innb_patch, by = c("bydel", "ar")) %>%
  mutate(
    befolkning = coalesce(befolkning, befolkning_patch)
  ) %>%
  select(-befolkning_patch)


bydel_all_pop %>%
  filter(ar %in% c(1995, 1997, 1999)) %>%
  summarise(
    n_rader = n(),
    andel_na_bef = mean(is.na(befolkning))
  )


bydel_all_pop


saveRDS(bydel_all_pop, "data_intermediate/bydel_ar_repr_allpartier_pop.rds")
