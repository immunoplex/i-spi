# I-SPI

A Shiny-based web application for analyzing and managing serology assay data with multi-user project management.

---

## Overview

**I-SPI** is an interactive R Shiny application for processing, analyzing, and visualizing several serology assay types including bead-based immunoassay data, e.g. from ELISA, MSD and Luminex. It provides a unified platform for managing serology experiments with robust features for data import, quality control, curve fitting, and results visualization.

---

## Features

### User Management & Projects
- **Secure Authentication:**  DEX OIDC integration via `app.R` and `auth_config.R`
- **Multi-User Support:** Role-based access (owner/collaborator) via `user_management.R`
- **Project Workspaces:** Isolated environments, switch & manage via `ui_handler.R`
- **Access Control:** Share projects with secure keys (UUID), validated in `user_management.R`

### Data Import & Processing
- **Multi-format Import:** Handles various bead array file types (`import_lumifile.R`)
- **Multi-Plate Support:** Organize and analyze multi-plate experiments (`combined_plates.R`)
- **Data Validation:** Built-in QC checks (`reset_import_values`)

### Analysis & Visualization
- **Blank Controls:** Background subtraction or sample gating
- **Bead Count Metrics:** Quality checks (`bead_count_functions.R`)
- **Curve Fitting:** Advanced algorithms (`standard_curve_ui.R`)
- **Fit Diagnostics:** Data > Results > Fit diagnostics assesses a fitted experiment (sampler health, response scale, settings provenance, plate heterogeneity, gates) and suggests next steps; downloadable as Markdown/JSON for an LLM. Design: `docs/design/fit_diagnostics.md`
- **Dilution Analysis:** Comprehensive dilution series analysis (`dilution_analysis_ui.R`)
- **Dilutional Linearity Correction:** Interactive method for computing corrected interpolated values comparable between samples run at different dilutions. ('dilutional_linearity_ui.R')
- **Outlier Detection:** Statistical outlier analysis (`outliers.R`)
- **Interactive UI:** Dynamic tables, collapsible panels, real-time updates, export support

---

## Technical Architecture

- **Backend:**
  - **Database:** PostgreSQL with schemas for users, results, and outliers ([`db_outliers.sql`](db_scripts/db_outliers.sql))
  - **Robust DB Management:** Transaction support, error handling, notifications
- **Frontend:**
  - **Shiny:** Main app in `src/app.R`
  - **Responsive UI:** Bootstrap, dynamic panels, interactive DT tables

---

## File Structure

    src/
    ├── app.R                 # Main app entry point
    ├── auth_config.R         # Dex OIDC config
    ├── user_management.R     # User & project operations
    ├── ui_handler.R          # UI rendering/event handling
    ├── import_lumifile.R     # Data import logic
    ├── outliers.R            # Outlier detection
    ├── curve_calc.R          # Curve fitting
    ├── *_ui.R                # UI modules
    └── *_functions.R         # Analysis/processing functions

---

## Database Schema

Defined in [`db_scripts/db_outliers.sql`](db_scripts/db_outliers.sql):

- **i_spi_users:** User, project, and permission management  
- **i_spi_results:** Experimental data and results  
- **i_spi_outliers:** Outlier analyses and context

---

## Getting Started

### Prerequisites
- R (≥ 4.0)
- PostgreSQL
- Required R packages (see source files)

### Setup

1. Set environment variables in `.env`

### Run the Application

    # Install dependencies
    # Configure database & Dex OIDC (Ability to disable it by setting flag = 1 Sys.setenv(LOCAL_DEV = "1"))
    shiny::runApp("src/app.R")

---

## Docker Support

- **Dockerfile:** Build application image
- **docker-compose.yml:** Orchestrate services

---

## Security Issues
If you uncover a security vulnerability in this app or in its containerization, please do not create a public notice and description, but instead contact Michael.S.Zens@dartmouth.edu and he will return your email as soon as possible. If necessary our I-SPI team will provide a full team response to address this type of problem swiftly and rigorously.

---

## Contributing

See the section: [CONTRIBUTING](CONTRIBUTING.md)

---

**For questions, contributions, or issues, please open a GitHub issue or pull request!**
