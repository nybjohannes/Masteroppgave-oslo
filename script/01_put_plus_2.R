library(dplyr)
library(stringr)
library(tibble)


# 1) Les inn data

vl     <- readRDS("data_final/valglister_golden_current.rds")
mandat <- readRDS("data_final/mandat_oslo_tidy.rds")



# 2) Parti-mapping slik at det blir riktig: vl$parti -> mandat$Parti
#dette er kun partier som har vunnet mandat i et lokal eller stortingsvalg i analyseperioden


parti_map <- tibble(
  parti = c(
    "arbeiderpartiet",
    "høyre",
    "fremskrittspartiet",
    "sosialistisk venstreparti",
    "kristelig folkeparti",
    "venstre",
    "rødt",
    "rød valgallianse",
    "senterpartiet",
    "miljøpartiet de grønne",
    "pensjonistpartiet",
    "partiet sentrum",
    "folkeaksjonen nei til mer bompenger"
  ),
  Parti = c(
    "Arbeiderpartiet",
    "Høyre",
    "Fremskrittspartiet",
    "Sosialistisk Venstreparti",
    "Kristelig Folkeparti",
    "Venstre",
    "Rødt",
    "Rød Valgallianse",
    "Senterpartiet",
    "Miljøpartiet de Grønne",
    "Pensjonistpartiet",
    "Partiet Sentrum",
    "Folkeaksjonen Nei til mer Bompenger"
  )
)

# Slå inn mappingen på valgliste-data
vl_map <- vl %>%
  left_join(parti_map, by = "parti")

# Om man vil sjekke om det har gått med alle:
# vl_map %>% count(is.na(Parti))


# 3) Lager tabell med "mandater_forrige" per parti og år
#    Altså  mandat i år t -> brukes som "forrige valg" for år t+4, +4 for å få til samme type valg


mandat_prev <- mandat %>%
  mutate(
    ar = aar + 4L   # valgåret der disse mandatene brukes som "forrige"
  ) %>%
  select(
    Parti,
    ar,
    mandater_forrige = mandater
  )

# Kan sjekke om det gikk:
# mandat_prev %>% arrange(Parti, ar) %>% head(20)


#Nå blir det litt skumlere heheh
# 4) Join: kandidatdata + mandater_forrige
#    NB: Jeg joiner KUN på Parti + ar ,  prøvde med valg_type, det skapte trøbbel og krøll hehe


vl_join <- vl_map %>%
  left_join(
    mandat_prev,
    by = c("Parti", "ar")
  )

# Kan her teste om det funka med AP
# vl_join %>%
#   filter(parti == "arbeiderpartiet") %>%
#   group_by(ar) %>%
#   summarise(mandater_forrige_uniq = unique(mandater_forrige)) %>%
#   arrange(ar)



# 5) Beregner så PUT+2 og filtrer til kandidater som teller


vl_put <- vl_join %>%
  mutate(
    mandater_forrige = if_else(is.na(mandater_forrige), 0L, as.integer(mandater_forrige)),
    put2_cutoff      = mandater_forrige + 2L
  ) %>%
  filter(plass <= put2_cutoff)

# Nå har vi KUN kandidatene som er innenfor PUT+2, altså toppkandidatene

#  om jeg vil ha nøyaktig samme variabler som vl (pluss to ekstra), kan jeg gjøre slik:
vl_put_out <- vl_put %>%
  select(
    valg_type, ar, parti, plass, kandidat_navn, fodselsar,
    postnr_num, poststed, bydel, omrade, indre_ytre,
    ost_vest, kandidat_id, kilde_fil,
    mandater_forrige, put2_cutoff   # legger igjen disse to, de er nyttige
  )


# 6) Lagrer "put"-datasettet, kaller det golden fordi dette funka endelig


saveRDS(vl_put_out, "data_final/valglister_golden_put.rds")
