# Masteroppgave Oslo

Dette repositoriet inneholder R-script og datasett som er brukt i masteroppgaven:

**Hvor er’u bor hen a? En kvantitativ undersøkelse av geografisk representasjon på valglistene og sammenhengen med valgdeltakelsen i Oslo**

Masteroppgave i statsvitenskap, Universitetet i Oslo, våren 2026.

## Innhold

Repositoriet inneholder datagrunnlag og script brukt til databehandling, figurer og analyser i oppgaven.

## Mappestruktur

### `data/`

Inneholder datasett brukt i analysene.

De viktigste filene er:

- `valglister_golden_current.rds` / `.csv`  
  Endelig kandidatdatasett med alle kandidater på valglistene i Oslo.

- `valglister_golden_put.rds` / `.csv`  
  Datasett med toppkandidater etter Put+2-operasjonaliseringa fra oppgaven.

- `bydel_ar_repr_allpartier_pop.rds` / `.csv`  
  Bydel-år-datasett med "representasjonspoeng" og befolkning.

- `valgdeltakelse.xlsx`, `utdanning.xlsx`, `innvandring.xlsx`, `inntekt.xlsx`  
  Bydelsvise kontroll- og utfallsdata brukt i den multivariate analysen.

### `script/`

Inneholder R-script brukt til databehandling, figurer og analyser.

Sentrale script:

- `lage put.R`  
  Lager datasettet med toppkandidater etter Put+2-operasjonaliseringen.

- `lage bydel-år data.R`  
  Lager bydel-år-datasett med nominell representasjon og toppkandidater.

- `få inn befolkning.R`  
  Kobler befolkningsdata til representasjonsdata.

- `Figurer kapittel 4.R`  
  Lager figurer til den deskriptive analysen.

- `Kart kapittel 4.R`  
  Lager kart til kapittel 4.

- `kart appendiks parti.R`  
  Lager partivise kart til appendiks.

- `multivariat analyse.R`  
  Kjører de multivariate analysene i kapittel 5.

- `teoretisk modell.R`  
  Lager teoretisk modell brukt i oppgaven.

## Filformater

Flere datasett ligger både som `.rds` og `.csv`. De bydelsspesifikke dataene ligger som `.xlsx`.

R-skriptene bruker primært `.rds`-filene. CSV-filene er lagt ved for enklere innsyn utenfor R.

## Merk

Prosjektet bygger på et egenutviklet datasett over valglister i Oslo fra 1987 til 2025. Deler av datagrunnlaget er manuelt digitalisert fra historiske valglister, statistiske årbøker og andre offentlige kilder.

Scriptene er først og fremst ment som dokumentasjon av databehandling, figurproduksjon og analyser brukt i masteroppgaven.
