## Add new strategy to the app

Steps to add new strategy to the app:

1. Create Flex report template in Interactive Brokers Client Portal. Include all possible data in the flex report. Since, flex report can be created only for the last 365 calendar days, make sure you create multiple files if date range for the strategy (sub account) is greater than 365 days. I create flex report for each year. For current year, I recreate flex report every day.
2. In the imports.R file set urls where above xml documents (flex reports) can be downloaded. Example for one strategy:
```
FLEX_MINMAX = c(
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_2022.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_2023.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax.xml"
)
```
3. Define start date in imports.R file after the above code. Example:
```
# strategies start
minmax_start = indicators_minmax[, min(as.Date(time))]
pra_start = indicators_pra[, min(as.Date(time))]
exuber_start = as.Date("2024-05-01")
exuber_old_start = as.Date("2024-01-02")
```
4. Add new strategy to the app.R file. Example:
```
nav_panel("MinMax", module_strategy_ui("minmax", alpha_ui_minmax)),
nav_panel("PRA", module_strategy_ui("pra", alpha_ui_pra)),
nav_panel("Exuber", module_strategy_ui("exuber", alpha_ui_exuber)),
```
5. Add new strategy to the app.R server. Example:
```
  portfolio_stats_pra = module_strategy_server(
    "pra",
    FLEX_PRA,
    start_date = pra_start,
    end_date = NULL,
    alpha = alpha_server_pra
```

6. Optionally, add alpha navtab panel to the app to explain alpha in detail. Steps to add slpha navtab panel to specific strategy are:

- Crete new R script with name alpha_{strteagy name}.R
- Copy paste content from alpha_pra.R file.
- Change all pra words with new strategy name (say exuber).
- make all necessary changes.
- source alpha_{strtegy name}.R file in app.R
- make sure you have added new alpha in app.R ui like this `nav_panel("MinMax", module_strategy_ui("minmax", alpha_ui_minmax)),` and in the app.R server like this `module_strategy_server("minmax", alpha_server_minmax)`.


