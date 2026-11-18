# Doxycycline Safety Shiny app — initial implementation

Based on the supplied historical `StudyShiny` app (bslib navbar, filters, DT tables, plots, downloads).

## Files
- `global.R`, `ui.R`, `server.R`, `functions.R`, `background.md`
- `data/`: place exported aggregate CSVs from each database here (subfolders supported).

## Run
Install: shiny, bslib, dplyr, tidyr, ggplot2, DT, omopgenerics, visOmopResults, readr, tibble.
From the app folder run `shiny::runApp('.')`.

## Important limitations before deployment
- No real CSVs were supplied. Import mappings and settings-dependent filters must be tested against actual exports.
- No synthetic results are displayed as real findings; missing results display empty tables/messages.
- The PS density currently displays only exported unweighted curves; weighted density needs its own export.
- SMDs use exported percentage/mean/SD, with unweighted reference SD. Multi-category SMD is per level.
- Missing/suppressed values are not reconstructed. Review complementary disclosure suppression before publishing.
- For a production-ready app, join result settings by result_id on import to label weighting correctly, and improve model filtering in forest plots.
- `omopViewer` may be used for further formatting; this initial app uses bslib/DT/ggplot2 for adaptable controls.
