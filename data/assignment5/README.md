# Assignment 5 supporting files

The portfolio report is `assignments/assignment5.qmd`, adapted from Team Mahjong's report at commit `d85d8b55097d859dbf5ee353af54958415758e99`.
The analysis and AI statement are preserved. `source_manifest.json` records the source files and path adaptations.

From the portfolio root:

```sh
quarto render assignments/assignment5.qmd
Rscript data/assignment5/verify_model.R
quarto render assignments/index.qmd
```

The cached ward boundaries, ACS estimates and robbery records allow the same analysis to run without downloading external data again. `references/` contains the original fold/model object and North/South comparison predictions used by the fitting script's reproducibility checks.

The two prediction CSVs are written here. `team_Mahjong_predictions.csv` is linked from the portfolio page.
