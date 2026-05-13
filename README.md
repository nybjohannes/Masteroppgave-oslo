# Masteroppgave Oslo

Dette repositoriet inneholder R-skript og datasett brukt i masteroppgaven:

**Hvor er’u bor hen a? En kvantitativ undersøkelse av geografisk representasjon på valglistene og sammenhengen med valgdeltakelsen i Oslo**

Masteroppgave i statsvitenskap, Universitetet i Oslo, våren 2026.

## Innhold

Repositoriet inneholder datagrunnlag og R-skript brukt til databehandling, figurer og analyser i oppgaven.

## Mappestruktur

### `data/`

Inneholder datasett brukt i analysene.

De viktigste filene er:

- `valglister_golden_current.rds` / `.csv`  
  Endelig kandidatdatasett med alle kandidater på valglistene i Oslo.

- `valglister_golden_put.rds` / `.csv`  
  Datasett med toppkandidater etter Put+2-operasjonaliseringen brukt i oppgaven.

- `bydel_ar_repr_allpartier_pop.rds` / `.csv`  
  Bydel-år-datasett med representasjon og befolkning.

- `valgdeltakelse.xlsx`, `utdanning.xlsx`, `innvandring.xlsx`, `inntekt.xlsx`  
  Bydelsvise kontroll- og utfallsdata brukt i den multivariate analysen.

### `script/`

Inneholder R-skript brukt til databehandling, figurer og analyser.

Sentrale skript:

- `01_put_plus_2.R`  
  Lager datasettet med toppkandidater etter Put+2-operasjonaliseringa forklart i oppgaven.

- `02_lage_bydel_ar_data.R`  
  Lager bydel-år-datasett med nominell representasjon og toppkandidater.

- `03_fa_inn_befolkning.R`  
  Kobler befolkningsdata til representasjonsdata.

- `04_figurer_kapittel_4_deskriptiv.R`  
  Lager figurer til den deskriptive analysen.

- `05_kart_kapittel_4.R`  
  Lager kartene brukt i kapittel 4.

- `06_kart_appendiks_parti.R`  
  Lager partivise kart brukt i appendiks.

- `07_multivariat_analyse.R`  
  Kjører de multivariate analysene i kapittel 5, og lager tabellene.

- `08_teoretisk_modell.R`  
  Lager visuell teoretisk modell brukt i oppgaven.

- `09_kart_oslo_teori.R`  
  Lager kartene brukt i teorikapitlet.

## Filformater

Flere datasett ligger både som `.rds` og `.csv`. Enkelte bydelsvise datasett ligger som `.xlsx`.

R-skriptene bruker primært `.rds`-filene. CSV-filene er lagt ved for enklere innsyn utenfor R.

## Merk

Prosjektet bygger på et egenutviklet datasett over valglister i Oslo fra 1987 til 2025. Deler av datagrunnlaget er manuelt digitalisert fra historiske valglister, statistiske årbøker og andre offentlige kilder.

Skriptene er først og fremst ment som dokumentasjon av databehandling, figurproduksjon og analyser brukt i masteroppgaven.
