# BC Community SES Index — Data Preparation

This context covers the cleaning, suppression, and delivery of source datasets that feed the BC community Socio-Economic Status index. It produces cleaned, catalogue-ready files; it does **not** compute the index itself.

## The index

**SES (Socio-Economic Status)**:
The relative social and economic standing of individuals or groups within BC, used to study disparities in health, education, and employment.
_Avoid_: SEI, "socioeconomic score."

**SEI (Socio-Economic Index)**:
The computed composite index that ranks geographies by SES. This repo builds its **inputs** only; the index is produced elsewhere, in the secure research environment.
_Avoid_: Using SEI and SES interchangeably — SES is the underlying status; SEI is the derived ranking.

## Time and data vintage

**Data year**:
The vintage year of the SES source data (census, crime, income, etc.). Source data currently caps at **2024**; there is no 2025/2026 data yet.
_Avoid_: Confusing with snapshot year or work year.

**Snapshot year**:
The date a reference table was snapshotted, used only to resolve geography. A fresh snapshot does **not** mean newer SES data.
_Avoid_: Treating a snapshot date as a data year.

**Work year (refresh year)**:
The calendar year in which a refresh is performed. The "2026 update" is work done in 2026 on 2024-vintage data — **not** 2026 data.

## Geography

**DA (Dissemination Area)**:
The smallest standard census geography; the base unit most datasets are resolved to.

**CSD (Census Subdivision)**:
A census-level municipality or region. Suppressed when it is an indigenous geography.

**CHSA (Community Health Service Area)**:
A BC health geography; some datasets are delivered at this level.

**RESP (Police Services Respondent Area)**:
A BC Stats sub-provincial geography representing a police-service reporting area. In TMF extracts the RESP code is a lookup key, translated to the area name via lookup tables; DAs are resolved to RESP through the TMF.

**GCS (GeoCoding Self-Service)**:
BC Stats' geocoding service that assigns a postal code or address to its corresponding sub-provincial geographies — health authorities, school districts, provincial electoral districts, RESP, and others.

**TMF (Translation Master File)**:
The fact table — sourced from GCS — that links geography levels (DA → RESP, etc.). Each refresh points at a dated snapshot of it.

## Pipeline stages

**Cleaning**:
Per-source scripts that download and convert one external dataset into cleaned files. The core purpose of this repo.

**Suppression**:
Removing or masking geographies for disclosure control before publication.

**Delivery**:
Preparing catalogue- or downstream-ready outputs from the cleaned, suppressed data.

**Explore** (out-of-pipeline):
Analysis and experimental applications kept for reference, not part of the cleaning pipeline.

## Wildfire sources

**Catalogue perimeter (BCDC)**:
Wildfire perimeters published to the BC Data Catalogue; the source that updates first after a fire season.
_Avoid_: Treating the catalogue pull as superseded — it is the fresher source whenever the local file lags.

**Local perimeter file**:
The geodata team's wildfire perimeter file; the source delivered wildfire outputs are built from, refreshed after the catalogue.

## Disclosure control

**Geo-suppression**:
Removing indigenous geographies — Indian reserves, Nisga'a lands, Indian government districts, and Nisga's regions — and masking small-count entries so published files meet disclosure rules.

## Publication

**BC Data Catalogue**:
The publication channel for delivered files. Outputs are delivered through the catalogue.
