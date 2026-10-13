# Installasjon av Meta-modellen og plassering av datalag

Kontrollert mot Metas offisielle `HighResCanopyHeight`-README 10. september
2026. Meta-repositoriet ble sist oppdatert 10. april 2025.

## 1. Automatisk oppsett i Windows PowerShell

Åpne PowerShell i `indicators\NO_GJEN_002` og kjør:

```powershell
Set-ExecutionPolicy -Scope Process Bypass
& .\SETUP_META_MODELL.ps1
```

Skriptet:

1. kloner `https://github.com/facebookresearch/HighResCanopyHeight.git` til
   `external\HighResCanopyHeight`;
2. registrerer den eksakte Git-revisjonen i `config.yml`;
3. laster bare ned de to nødvendige vektene fra Metas offentlige S3-bøtte;
4. oppretter Conda-miljøet `no_gjen_002` med Python 3.9;
5. installerer PyTorch 2.0.1 og torchvision 0.15.2 for CPU samt øvrige pakker;
6. registrerer riktig `python.exe` i `config.yml`.

Den valgte `compressed_SSLhuge_aerial.pth` er laget for flybilder. Meta oppgir
at komprimerte modeller skal kjøres på CPU; GPU er ikke testet for disse.

Hvis Git, AWS CLI eller Miniconda mangler, stopper skriptet og viser den
aktuelle `winget`-kommandoen. Start PowerShell på nytt etter installasjon.

Offisielle kilder:

- kode og installasjon: https://github.com/facebookresearch/HighResCanopyHeight
- offentlige modeller: `s3://dataforgood-fb-data/forests/v1/models/`
- modellisens: Apache License 2.0

Det er ikke nødvendig å hente `data.zip` for vår norske inferens.

## 2. Modellfiler som skal finnes

```text
external/HighResCanopyHeight/
  models/
  saved_checkpoints/
    compressed_SSLhuge_aerial.pth
    aerial_normalization_quantiles_predictor.ckpt
```

## 3. Ortofoto

Dette finnes allerede gjennom landsjobben:

```text
C:/nib-norge/landsdekkende_2m/
  block_x..._y..._50km/
    nib_AAR_xX_yY.jpg
    nib_AAR_xX_yY.jgw
    flisregister.csv
```

Kilden er EPSG:25833, 2 m, 1000 × 1000 piksler og 2 × 2 km per flis. Nyeste
ordinære ortofoto brukes per piksel; eldre ordinære bilder fyller NoData.
Skråfoto, satellitt og IR er utelukket i nedlastingsjobben.

## 4. NINA-datalag

Legg produksjonsklare kopier i prosjektets `data`-mappe, eller endre stiene i
`config.yml`.

| Formål | Målfil | Påkrevde felt | Kjent kilde/kandidat |
|---|---|---|---|
| Analyse-/populasjonspolygoner | `data/AOIs.shp` | `id`, `ecosystem` | Den gamle koden brukte prosjektfilen `AOIs.shp`; autoritativ nasjonal GRUK/ASO-populasjon må velges |
| Fem rapporteringsregioner | `data/regions.shp` | `region_id`, `region_name` | Den gamle koden brukte prosjektfilen `regions.shp`; ingen serversti var dokumentert |
| Moens bioklimatiske soner | `data/bioclimatic_zones.gpkg` | `vegClimZone` | `R:/GeoSpatialData/BiogeographicalRegions/Norway_VegetationZones_Moen/Original/Vector/soner.shp`; feltet het opprinnelig `KLASSE` |
| God tilstand | `data/reference_good.csv` | `ecosystem`, `region_id`, `bioclimatic_zone`, `reference_height_m` | Må samles fra prosjektfilene `refvaatmark.csv`, `refaapne.csv`, `refsemi.csv` og kalibreres mot Meta-resultat |
| Moden skog | `data/reference_forest.csv` | `ecosystem`, `region_id`, `bioclimatic_zone`, `forest_height_m` | Kandidat: `P:/412421_okologisk_tilstand_2024/Ida/From_GEE/vegHeights_skog_climZoneRegion.csv`; må omformes og proveniensgodkjennes |

Alle romlige lag transformeres til EPSG:25833 av pipelinen. Feltmapping kan
endres under `fields:` i `config.yml`.

De to referansetabellene i tabellen er **ikke ferdige datakilder**. Dagens
LiDAR-baserte verdier er kun provisoriske for NO_GJEN_002 og må rekalibreres
med flyfotomodellen før publisering.

## 5. Kontroll og første test

```powershell
& .\KONTROLLER_OPPSETT.ps1
& .\START_NO_GJEN_002.ps1 -Stage validate
& .\START_NO_GJEN_002.ps1 -Stage manifest -Overwrite
```

Bruk først et lite AOI-utvalg. Etter kontroll av manifestet:

```powershell
& .\START_NO_GJEN_002.ps1 -Stage inference
```

Inferensen er gjenopptakbar. Ikke bruk `-Overwrite` når ferdige prediksjoner
skal beholdes.

## 6. Metodisk stoppunkt

Meta-opplegget er testet ved 0,5 og 1 m i NO_GJEN_002-arbeidet. Det nasjonale
arkivet er 2 m. Koden støtter en eksplisitt eksperimentell test, men nasjonale
indikatorverdier skal ikke rapporteres før 2-metersestimatene er sammenlignet
med LiDAR/feltdata og referanseverdiene er rekalibrert.
