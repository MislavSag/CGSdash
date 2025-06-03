## Add new strategy to the app

Steps to add new strategy to the app:

1. Create Flex report template in Interactive Brokers Client Portal. 
Include all possible data in the flex report. 
Since, flex report can be created only for the last 365 calendar days, 
make sure you create multiple files if date range for the strategy (sub account) is greater than 365 days. 
I create flex report for each year. For current year, I recreate flex report every day.

2. Add lex report genration on server in project Quantdata in file script_flex.R.

3. Add indicators for new strategy from DO.

4. In the imports.R file set urls where above xml documents (flex reports) can be downloaded. Example for one strategy:
```
FLEX_MINMAX = c(
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_2022.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_2023.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax.xml"
)
```
5. Define start date in imports.R file after the above code. Example:
```
# strategies start
minmax_start = indicators_minmax[, min(as.Date(time))]
pra_start = indicators_pra[, min(as.Date(time))]
exuber_start = as.Date("2024-05-01")
exuber_old_start = as.Date("2024-01-02")
```
6. Add new strategy to the app.R file. Example:
```
nav_panel("MinMax", module_strategy_ui("minmax", alpha_ui_minmax)),
nav_panel("PRA", module_strategy_ui("pra", alpha_ui_pra)),
nav_panel("Exuber", module_strategy_ui("exuber", alpha_ui_exuber)),
```
7. Add new strategy to the app.R server. Example:
```
  portfolio_stats_pra = module_strategy_server(
    "pra",
    FLEX_PRA,
    start_date = pra_start,
    end_date = NULL,
    alpha = alpha_server_pra
```

8. Add strategy to the home page table, os it can be quickly compares to benchmark and other strategies.
First add date here

```
    dates = c(o_pra()$date,
              o_minmax()$date,
              o_riskcombo()$date,
              o_exuber()$date,
              o_lv()$date) # , as.Date("2020-01-01")
```
than add portfolio stats here
```
    dt_ = cbind(
      get_portfolio_stats_from_strategy(o_pra()$strategy, o_pra()$date, leverage_),
      get_portfolio_stats_from_strategy(o_minmax()$strategy, o_minmax()$date, leverage_),
      get_portfolio_stats_from_strategy(o_riskcombo()$strategy, o_riskcombo()$date, leverage_),
      get_portfolio_stats_from_strategy(o_exuber()$strategy, o_exuber()$date, leverage_),
      get_portfolio_stats(rbi_lv)
    )
```
Also add new line here
    dt_portfolio(dt_, "strategy_performance", dates) |>
      formatStyle(columns = 2:3, backgroundColor = "#f0f8ff") |>
      formatStyle(columns = 4:5, backgroundColor = "#f0fff0") |>
      formatStyle(columns = 6:7, backgroundColor = "#ffe4e1") |>
      formatStyle(columns = 8:9, backgroundColor = "#f0f8ff") |>
      formatStyle(columns = 10:11, backgroundColor = "#f0fff0")

Finally, to make this table work you should also change dt_portfolio function in imports.R file. 
there are notes what to change. You should add here

```
        tr(
          th('Portfolio Statistics'),
          th('Strategy PRA'),    th('Benchmark'),
          th('Strategy MinMAx'), th('Benchmark'),
          th('Strategy Exuber'), th('Benchmark'),
          th('Strategy LV'), th('Benchmark'),
          th('Strategy Risk Combo'), th('Benchmark')
          # ADD HERE
        )
```
and here
```
              "  var targetIndecies = [2, 3, 4, 5, 8, 9, 10];", # CHANGE HERE
                "  if (targetIndecies.includes(index)) {",
                "    for (var i = 1; i < data.length; i++) {",
                "      var num = parseFloat(data[i]).toFixed(2);",
                "      $('td:eq(' + i + ')', row).html(num);",
                "    }",
                "  }",
                "}"
```

9. Optionally, add position status of the strategy. by changin those two parts in app.R:

```
    # Check each module's `portfolio_has_asset`
    has_pra    = o_pra()$portfolio_has_asset
    has_minmax = o_minmax()$portfolio_has_asset
    has_riskcombo = o_riskcombo()$portfolio_has_asset
    has_exuber = o_exuber()$portfolio_has_asset
    has_exuber_old <- portfolio_stats_exuber_old()$portfolio_has_asset
    has_exuber_total <- portfolio_stats_exuber_total()$portfolio_has_asset
    has_lv      <- o_lv()$portfolio_has_asset

    # Build a small info string
    # For example, you can keep a named vector of bools:
    # Named logical vector
    strategy_status <- c(
      "PRA"          = has_pra,
      "MinMax"       = has_minmax,
      "Exuber"       = has_exuber,
      "Least Vol"    = has_lv,
      "RiskComobo"   = has_riskcombo
    )

```

10. Optionally, add alpha navtab panel to the app to explain alpha in detail. Steps to add slpha navtab panel to specific strategy are:

- Crete new R script with name alpha_{strteagy name}.R
- Copy paste content from alpha_pra.R file.
- Change all pra words with new strategy name (say exuber).
- make all necessary changes.
- source alpha_{strtegy name}.R file in app.R
- make sure you have added new alpha in app.R ui like this 
`nav_panel("MinMax", module_strategy_ui("minmax", alpha_ui_minmax)),` 
and in the app.R server like this `module_strategy_server("minmax", alpha_server_minmax)`.


