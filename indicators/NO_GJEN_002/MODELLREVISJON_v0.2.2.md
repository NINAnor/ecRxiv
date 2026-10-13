# Modellrevisjon for NO_GJEN_002 v0.2.2

Kontrollert 10. september 2026 mot Metas publiserte kode og dokumentasjon:

- https://github.com/facebookresearch/HighResCanopyHeight
- https://github.com/facebookresearch/HighResCanopyHeight/blob/main/inference.py

## Resultat

| Komponent | Meta | Tidligere prosjektkode | v0.2.2 |
|---|---|---|---|
| RGB-skala | PIL `ToTensor`, tilsvarende uint8 / 255 | Rasterbånd / dtype-maksimum | Beholdt; JPEG uint8 blir /255 |
| Kanalrekkefølge | RGB | Rasterbånd 1,2,3 | Beholdt og eksplisitt kontrollert |
| Modellvindu | 256 × 256 | 256 × 256 | Beholdt |
| Målpercentiler | Predikert av `aerial_normalization_quantiles_predictor.ckpt` | Samme | Beholdt |
| Inngangspercentiler | p5/p95 | p20/p80 | Rettet til p5/p95 |
| Generell RGB-normalisering | mean 0.420/0.411/0.296; sd 0.213/0.156/0.143 | Samme | Beholdt |
| Komprimert modell | Dynamisk kvantisering, CPU | Samme | Beholdt |
| Høydeskala | `10 * raw prediction` | `9.51 * raw + 0.4` | Rettet til `10 * raw` |
| Negative høyder | ReLU ved evaluering | ReLU | Beholdt |

## Begrunnelse for rettingene

Den arvede 20/80-normaliseringen og lineære transformasjonen `9.51*x+0.4`
hadde ingen kilde, parameterestimat, datasettversjon eller valideringsrapport i
prosjektet. Begge kunne gi troverdige, men systematisk forskjøvede høyder.
Produksjonsstandarden følger derfor den publiserte Meta-implementasjonen.

Alle fire parametere ligger nå i `config.yml`:

```yaml
input_low_quantile: 0.05
input_high_quantile: 0.95
output_scale: 10.0
output_offset_m: 0.0
```

En framtidig norsk rekalibrering kan bruke andre verdier, men må ha et låst
kalibreringsdatasett, estimert usikkerhet og ny versjon av indikatoren.

## Reproduserbarhet og sikker gjenopptak

Hver inferenskjøring skriver `inference_provenance.json` med:

- Git-revisjon for Meta-koden;
- SHA-256 for begge checkpoint-filer;
- normaliserings- og høydeparametere;
- bildeoppløsning, vindusstørrelse, overlapp, CRS og programversjoner.

Før en eksisterende prediksjon hoppes over, kontrolleres filstørrelse,
rastermål, CRS, oppløsning og geografisk utstrekning. En fil fra feil
oppløsningskjøring blir dermed ikke stille gjenbrukt.

## Gjenstående faglige tester

- Kjør samme norske testbilder gjennom Metas originale program og v0.2.2 og
  kontroller numerisk samsvar uten overlapp.
- Sammenlign 0,5, 1 og 2 m med benchmarkpakken.
- Valider kronhøyder mot samtidige LiDAR- eller feltdata.
- Undersøk sesong, skygge, kamera og prosjektgrenser.
- Rekalibrer indikatorens referanseverdier med den låste flyfotomodellen.
