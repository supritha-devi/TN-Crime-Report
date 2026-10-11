# Tamil Nadu Crime Insight — Geospatial Analytics Dashboard

[![Shiny](https://img.shields.io/badge/R-Shiny-blue.svg)](https://shiny.posit.co/)
[![Geospatial](https://img.shields.io/badge/GIS-Leaflet%20%7C%20sf-green.svg)](https://r-spatial.github.io/sf/)
[![Status](https://img.shields.io/badge/Deployment-Live-success.svg)](https://supritha-devi.shinyapps.io/tn_crime_app_ready/)

An interactive geospatial crime analytics platform and decision-support dashboard for Tamil Nadu, India. The application combines official 2023 Tamil Nadu State Crime Records Bureau (TN-SCRB) data with 2011 Census demographics, sub-district (taluk) spatial geometries, k-means risk clustering (AI/ML), and an intelligent domain-specific chatbot engine.

---

## 🌐 Live Application
- **Live URL**: [https://supritha-devi.shinyapps.io/tn_crime_app_ready/](https://supritha-devi.shinyapps.io/tn_crime_app_ready/)

---

## 🚀 Key Features

- **Interactive State Choropleth Map**:
  - Full coverage of all 38 districts of Tamil Nadu.
  - Multi-category crime rate switching: Total (IPC+SLL), Murder, Crimes against Women, Road Accidents, and Suicides.
  - Dynamic quantile-based color scales with always-visible labels and interactive popups.

- **Taluk-Level Geospatial Drilldown**:
  - Sub-district (taluk) administrative boundaries for 32 districts (via GADM v4.1 Level-3).
  - Synchronized district locator mini-map showing relative geographic position within the state.

- **Role-Based Safety Reports**:
  - Contextual interpretations tailored to user roles: *Common Citizen, Police / Law Enforcement, Builder / Property Developer, Journalist / Media, NGO / Social Worker, Student / Researcher, Government Official*.
  - Comprehensive demographic integration (population, literacy rate, urbanization percentage).
  - Automated comparable-districts benchmark based on population parity.

- **Automated Report & Data Export**:
  - Single-click self-contained **HTML District Report** export with embedded base64 categorical visual charts (printable directly to PDF).
  - Raw data export in **CSV** and **JSON** formats.

- **AI/ML Crime Risk Clustering**:
  - Multi-dimensional k-means clustering on scaled crime rates across all categories.
  - Silhouette validation for optimal cluster count grouping districts into Low, Moderate, Elevated, and High-risk tiers.

- **Rule-Based Intelligent Chatbot**:
  - Natural-language query parsing with alias support (e.g., Madras → Chennai, Kovai → Coimbatore).
  - District lookups, highest/lowest rankings, multi-district comparisons, and role-conditioned guidance.
  - Built-in ethical guardrails: rejects speculation regarding named individuals or unverified cases.

- **Authentication & Activity History**:
  - User registration with custom visual avatar builder and salted SHA-256 password hashing.
  - Instant 1-Click Demo / Guest login mode for streamlined evaluations.
  - SQLite backend tracking individual search history and accessed district reports.
  - Dark Mode and Light Mode theme toggle.

---

## 🛠️ Tech Stack & Architecture

| Component | Technology / Library | Purpose |
|---|---|---|
| **Framework** | R, Shiny, shinyjs | Reactive web application framework & DOM manipulation |
| **Mapping & GIS** | `leaflet`, `sf`, `terra`, `raster` | Vector spatial processing, GeoJSON parsing, interactive choropleths |
| **Data Manipulation** | `dplyr`, `readr`, `stringr`, `jsonlite` | Data transformation, filtering, and JSON interchange |
| **Database & Storage** | SQLite (`RSQLite`, `DBI`) | Persistent user storage, authentication, and history tracking |
| **Security** | `digest` | Salted SHA-256 cryptographic password hashing |
| **Visualizations** | `ggplot2`, `base64enc`, `DT` | Interactive tables and dynamic base64 report graphics |
| **Clustering (AI/ML)**| `stats` (k-means), `cluster` | Unsupervised risk profile categorization and silhouette validation |

---

## 📂 Project Structure

```text
tn_crime_app_ready/
├── app.R                                # Main Shiny application (UI & Server)
├── tn_crime_app.Rproj                   # RStudio project configuration
├── R/
│   ├── chatbot_engine.R                 # Rule-based NLP and query engine
│   └── report_builder.R                 # Self-contained HTML report generator
├── data/
│   ├── tn_crime_master.csv              # Cleaned 38-district crime & demographics master table
│   ├── tn_crime_map_ready.geojson       # District boundary polygon geometries
│   └── tn_taluks.geojson                # Sub-district taluk boundary geometries
├── clustering/
│   └── 02_optional_clustering.R         # K-means unsupervised clustering pipeline
├── db/
│   └── tn_app.db                        # SQLite database for accounts and activity history
└── www/
    └── styles.css                       # Custom CSS styling (Light/Dark themes, responsive cards)
