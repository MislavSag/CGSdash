library(shiny)
library(ibflexr)
library(DT)
library(dygraphs)
library(xml2)
library(DBI)
library(data.table)
library(bsicons)
library(RPostgres)
library(lubridate)
library(PerformanceAnalytics)
library(quarto)
library(yaml)
library(htmltools)
library(waiter)
library(bslib)
library(httr)


# SET UP ------------------------------------------------------------------
# Connect to database
connec = dbConnect(
  RPostgres::Postgres(),
  dbname = "defaultdb",
  host = "db-postgresql-fra1-54406-do-user-13776848-0.c.db.ondigitalocean.com",
  port = 25060L,
  user = "doadmin",
  password = "AVNS_7h0PktF6BbOHWOUK45K"
)


# MINMAX INDICATORS -------------------------------------------------------
# Indicator values from logs
logs = "
2023-02-10 15:00:00 0.011681145000756475
2023-02-10 16:00:00 0.011681145000756475
2023-02-10 17:00:00 0.011681145000756473
2023-02-10 18:00:00 0.011681145000756475
2023-02-10 19:00:00 0.011681145000756473
2023-02-10 20:00:00 0.011681145000756475
2023-02-10 21:00:00 0.017864628004722143
2023-02-13 15:00:00 -0.011800809609643048
2023-02-13 16:00:00 -0.01180080960964305
2023-02-13 17:00:00 -0.01180080960964305
2023-02-13 18:00:00 -0.011800809609643048
2023-02-13 19:00:00 -0.011800809609643047
2023-02-13 20:00:00 -0.01180080960964305
2023-02-13 21:00:00 -0.011111844687481928
2023-02-14 15:00:00 -0.010978820712341327
2023-02-14 16:00:00 -0.010978820712341327
2023-02-14 17:00:00 -0.010978820712341327
2023-02-14 18:00:00 -0.010978820712341327
2023-02-14 19:00:00 -0.010978820712341327
2023-02-14 20:00:00 -0.058022819911335566
2023-02-14 21:00:00 -0.05390246347060714
2023-02-15 15:00:00 0.0036659871989291287
2023-02-15 16:00:00 0.003665987198929138
2023-02-15 17:00:00 -0.05399230494622139
2023-02-15 18:00:00 0.0027993984095100507
2023-02-15 19:00:00 -0.05497669496114871
2023-02-15 20:00:00 -0.05537040057355189
2023-02-15 21:00:00 0.0033009383033132202
2023-02-16 15:00:00 0.006477360159233874
2023-02-16 16:00:00 -0.05133560725170316
2023-02-16 17:00:00 -0.005581545187280765
2023-02-16 18:00:00 -0.005581545187280761
2023-02-16 19:00:00 -0.005581545187280755
2023-02-16 20:00:00 -0.05143112814387491
2023-02-16 21:00:00 -0.05157235500756082
2023-02-17 15:00:00 -0.050515607704043564
2023-02-17 16:00:00 -0.049376582105548836
2023-02-17 17:00:00 -0.049949745612805456
2023-02-17 18:00:00 -0.04994974561280544
2023-02-17 19:00:00 -0.049805281727045604
2023-02-17 20:00:00 -0.051081564957261336
2023-02-17 21:00:00 -0.052678353789496285
2023-02-21 19:00:00 -0.0541708032232994
2023-02-21 20:00:00 -0.05705230538989443
2023-02-21 21:00:00 -0.05304847206782718
2023-02-22 15:00:00 -0.056970208005096
2023-02-22 16:00:00 -0.05696357249550578
2023-02-22 17:00:00 -0.05696357249550575
2023-02-22 18:00:00 -0.056963572495505745
2023-02-22 19:00:00 -0.05696357249550578
2023-02-22 20:00:00 -0.05717761257608076
2023-02-22 21:00:00 -0.056409434210420564
2023-02-23 15:00:00 -0.056044846750426015
2023-02-23 16:00:00 -0.056044846750426
2023-02-23 17:00:00 -0.055383745112356705
2023-02-23 18:00:00 -0.055383745112356705
2023-02-23 19:00:00 -0.056086661649492775
2023-02-23 20:00:00 0.0033331202978616283
2023-02-23 21:00:00 0.0033331202978616075
2023-02-24 15:00:00 -0.05634948752658412
2023-02-24 16:00:00 -0.05634948752658411
2023-02-24 17:00:00 0.0014758725067841013
2023-02-24 18:00:00 -0.05700515088737586
2023-02-24 19:00:00 0.0013000474929120774
2023-02-24 20:00:00 0.0017093386139375906
2023-02-24 21:00:00 -0.05591449042436064
2023-02-27 15:00:00 0.007025035831321312
2023-02-27 17:00:00 0.007025035831321313
2023-02-27 18:00:00 0.0070250358313213145
2023-02-27 19:00:00 0.007025035831321313
2023-02-27 20:00:00 0.007025035831321315
2023-02-27 21:00:00 0.007025255625577275
2023-03-07 16:00:00 0.03203482528632645
2023-03-07 17:00:00 0.03203482528632645
2023-03-07 18:00:00 0.03203482528632645
2023-03-07 19:00:00 0.03203482528632645
2023-03-07 20:00:00 0.03203482528632645
2023-03-07 21:00:00 0.03203482528632645
2023-03-08 16:00:00 0.0970052452799304
2023-03-08 17:00:00 0.0970052452799304
2023-03-08 18:00:00 0.0970052452799304
2023-03-08 20:00:00 0.0970052452799304
2023-03-08 21:00:00 0.0970052452799304
2023-03-09 15:00:00 0.004868208959803898
2023-03-09 16:00:00 0.004868208959803898
2023-03-09 17:00:00 0.004868208959803898
2023-03-09 18:00:00 0.004868208959803898
2023-03-09 19:00:00 0.004868208959803898
2023-03-09 20:00:00 0.0048682089598039
2023-03-09 21:00:00 0.0048682089598039
"
df = read.table(text = logs, col.names = c("date", "time", "value"))
df = as.data.table(df)
df[, timestamp := as.POSIXct(paste(date, time), tz = "Europe/Warsaw")]
df[, timestamp := with_tz(timestamp, tz = "UTC")]
minmax_old = df[, .(timestamp, value)]

# set non=-working date
# minmax_error = data.table(
#   seq.Date()
# )
minmax_error = data.table(
  timestamp = c(
    seq(from=as.POSIXct("2023-02-28 16:00", tz="UTC"),
        to=as.POSIXct("2023-02-28 21:00", tz="UTC"),
        by="hour"),
    seq(from=as.POSIXct("2023-03-01 16:00", tz="UTC"),
        to=as.POSIXct("2023-03-01 21:00", tz="UTC"),
        by="hour"),
    seq(from=as.POSIXct("2023-03-02 16:00", tz="UTC"),
        to=as.POSIXct("2023-03-02 21:00", tz="UTC"),
        by="hour"),
    seq(from=as.POSIXct("2023-03-03 16:00", tz="UTC"),
        to=as.POSIXct("2023-03-03 21:00", tz="UTC"),
        by="hour"),
    seq(from=as.POSIXct("2023-03-06 16:00", tz="UTC"),
        to=as.POSIXct("2023-03-06 21:00", tz="UTC"),
        by="hour")
  ),
  value = NA_real_
)
minmax_old = rbind(minmax_old, minmax_error)
setorder(minmax_old, timestamp)

# minmax indicators
indicators_minmax = dbReadTable(connec, "indicators_minmax")
indicators_minmax = as.data.table(indicators_minmax)
indicators_minmax = indicators_minmax[as.Date(timestamp) > as.Date("2023-03-09")]
indicators_minmax = rbind(minmax_old, indicators_minmax[, .(timestamp, value)])
setnames(indicators_minmax, c("timestamp", "value"), c("time", "minmax_indicator"))
indicators_minmax[, threshold := -0.004]


# PRA INDICATORS ----------------------------------------------------------
# PRA indicators
indicators_pra = dbReadTable(connec, "indicators_pra")
indicators_pra = as.data.table(indicators_pra)
indicators_pra = indicators_pra[-1, c(1, 3)]
setnames(indicators_pra, c("timestamp", "value"), c("time", "pra_indicator"))


# EXUBER INDICATORS -------------------------------------------------------
# exuber indicators
indicators_exuber = dbReadTable(connec, "cgsexuberyieldtbl")
indicators_exuber = as.data.table(indicators_exuber)
indicators_exuber = dcast(indicators_exuber, timestamp ~ variable, value.var = "value")
setnames(indicators_exuber, "timestamp", "time")


# RISK COMBO INDICATORS ---------------------------------------------------
# risk combo indicators
indicators_risk_combo = dbReadTable(connec, "indicators_riskcombo")
indicators_risk_combo = as.data.table(indicators_risk_combo)
indicators_risk_combo = dcast(indicators_risk_combo, timestamp ~ variable, value.var = "value")
setnames(indicators_risk_combo, "timestamp", "time")


# ALPHA PICKS INDICATORS ---------------------------------------------------
# alpha picks indicators
indicators_alphapicks = dbReadTable(connec, "indicators_alphapicks")
indicators_alphapicks = as.data.table(indicators_alphapicks)
indicators_alphapicks = dcast(indicators_alphapicks, timestamp ~ variable, value.var = "value")
setnames(indicators_alphapicks, "timestamp", "time")

# VIX COMBO INDICATORS ---------------------------------------------------
# vix combo indicators
indicators_vix_combo = dbReadTable(connec, "indicators_vixcombo")
indicators_vix_combo = as.data.table(indicators_vix_combo)
indicators_vix_combo = dcast(indicators_vix_combo, timestamp ~ variable, value.var = "value")
setnames(indicators_vix_combo, "timestamp", "time")


# CLOSE CONNECTION --------------------------------------------------------
# close connection
dbDisconnect(connec)


# RBI STRATEGIES ----------------------------------------------------------
# Least Volatile
#rbi_lv = fread("https://snpmarketdata.blob.core.windows.net/rbi/least_volatile_prinosi.csv",
#               col.names = c("date", "strategy_ret", "benchmark_ret", "strategy", "benchmark"))
#rbi_lv_portfolio = fread("https://snpmarketdata.blob.core.windows.net/rbi/least_volatile_pozicije.csv")


# INPUTS -----------------------------------------------------------------
# Flex report urls for all strategies
FLEX_PRA = c(
  "https://snpmarketdata.blob.core.windows.net/flex/pra_2023.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/pra_old_account.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/pra_2024.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/pra.xml"
)
FLEX_MINMAX = c(
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_2022.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_2023.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_old_account.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax_2024.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/minmax.xml"
)
FLEX_EXUBER = c(
  "https://snpmarketdata.blob.core.windows.net/flex/exuberbondsml_2023.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/exuberv1_old_account.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/exuber_2024.xml",
  "https://snpmarketdata.blob.core.windows.net/flex/exuberv1.xml"
)
FLEX_RISKCOMBO = c(
  "https://snpmarketdata.blob.core.windows.net/flex/riskcombo.xml"
)
FLEX_ALPHAPICKS = c(
  "https://snpmarketdata.blob.core.windows.net/flex/alphapicks.xml"
)
FLEX_VIXCOMBO = c(
  "https://snpmarketdata.blob.core.windows.net/flex/vixcombo.xml"
)

# strategies start
# Old - my way - first indicator apparance
# minmax_start = indicators_minmax[, min(as.Date(time))]
# pra_start = indicators_pra[, min(as.Date(time))]
# exuber_start = as.Date("2024-05-01")
# exuber_old_start = indicators_exuber[, min(as.Date(time))]
# exuber_start_total = as.Date(2023-02-14)
# Hardcoded Andrea dates
minmax_start = as.Date("2022-11-21")
pra_start = as.Date("2023-03-22")
exuber_start = as.Date("2024-05-01")
exuber_old_start = indicators_exuber[, min(as.Date(time))]
exuber_start_total = as.Date("2023-02-14")
riskcombo_start = as.Date("2025-05-18")
alphapicks_start = as.Date("2025-04-16")
vixcombo_start = as.Date("2025-08-27")
#least_volatile_start = rbi_lv[, min(as.Date(date))]


# UTILS -------------------------------------------------------------------
# Data table template
myDT = function(df, filename, rownames = FALSE) {
  DT::datatable(
    df,
    rownames = rownames,
    extensions = c('Buttons', "FixedHeader"),
    options = list(
      dom = 'Brtpl',
      scrollX = TRUE,
      buttons = list(
        'copy',
        list(extend = 'csv', filename = filename),
        list(extend = 'excel', filename = filename),
        list(extend = 'pdf', filename = filename),
        'print'
        # list(extend = 'colvis', columns = c(1:(ncol(
        #   df
        # ) - 1)))
      )
    ),
  )
}

dt_portfolio = function(df, filename = "df", dates = NULL) {
  # Debug
  # df = get_portfolio_stats(rbi_lv)
  # dates = Sys.Date()

  # Convert df to data frame if it is xts
  if (is.matrix(df)) {
    df = as.data.table(df, keep.rownames = TRUE)
    colnames(df)[1] = 'Portfolio Statistics'
  }

  # Change column names for RBI
  if ("strategy_ret" %in% names(df)) {
    setnames(df, c("strategy_ret", "benchmark_ret"), c("LV", "Benchmark"))
  }

  if (!is.null(dates)) {
    sketch = htmltools::withTags(table(
      class = 'display',
      thead(
        tr(
          th("Start date"),
          lapply(dates, function(x) th(x, colspan = 2, style = "text-align: center;"))
        ),
        tr(
          th('Portfolio Statistics'),
          th('Strategy Risk Combo'), th('Benchmark'),
          th('Strategy Alpha Picks'), th('Benchmark'),
          th('Strategy VIX Combo'), th('Benchmark'),
          th('Strategy PRA'),    th('Benchmark'),
          th('Strategy MinMax'), th('Benchmark'),
          th('Strategy Exuber'), th('Benchmark')#,
          #th('Strategy LV'), th('Benchmark')
          # ADD HERE
        )
      )
    )
    )
  }
  datatable(df,
            rownames = FALSE,
            extensions = c('Buttons', "FixedHeader"),
            options = list(
              dom = 'Brt', # 'Brtpl'
              scrollX = TRUE,
              buttons = list(
                'copy',
                list(extend = 'csv', filename = filename),
                list(extend = 'excel', filename = filename),
                list(extend = 'pdf', filename = filename),
                'print'
              ),
              rowCallback = JS(
                "function(row, data, displayNum, index) {",
                "  if (index < 2 || index === 6 || index == 7) {", # CHANGE HERE IF NEW ROWS ADDED
                "  for (var i = 1; i <= data.length; i++) {",
                "      var num = (parseFloat(data[i]) * 100).toFixed(2) + '%';",
                "      $('td:eq(' + i + ')', row).html(num);",
                "  }",
                "  }",
                "",
                "  var targetIndecies = [2, 3, 4, 5, 8, 9, 10];", # CHANGE HERE COLUMNS ADDED
                "  if (targetIndecies.includes(index)) {",
                "    for (var i = 1; i < data.length; i++) {",
                "      var num = parseFloat(data[i]).toFixed(2);",
                "      $('td:eq(' + i + ')', row).html(num);",
                "    }",
                "  }",
                "}"
              )
            ),
            container = if (is.null(dates)) tags$table(tableHeader(colnames(df), TRUE),
                                                       class = "display") else sketch
  )
}

# dt_portfolio(portfolio_stats, "test")
# dt_portfolio(portfolio_stats, "test", dates = as.character(Sys.Date()))

# Help function to merge indicators and trades
merge_indicators_trades = function(indicators, trades, val = "value") {
  # indicators = copy(indicators_pra[, .(time, pra_indicator)])
  # trades = copy(trades_dt_pra_summary)
  # val = "pra_indicator"

  # Set timezones
  trades[, dateTime := force_tz(dateTime, tzone = "America/New_York")]
  indicators[, time := with_tz(time, tzone = "America/New_York")]

  # Merge indicators and trades
  indicators[, time_indicator := time]
  indicators_and_trades = indicators[trades, on = c("time" = "dateTime"), roll = "nearest"]
  indicators_and_trades = indicators_and_trades[, .(time, time_indicator, q, p)]

  # Merge now indicators and merged trades
  res = indicators_and_trades[indicators, on = c("time_indicator" = "time")]

  # Set indicator values for buy and sell
  res[q > 0, buy := get(val)]
  res[q < 0, sell := get(val)]

  # Keep unique
  res = unique(res, by = "time_indicator")

  return(res)
}

# set timezone for dygraph
set_timezone = function(dg, input = 1) {
  if (input == 1) {
    dg %>%
      dyOptions(includeZero = TRUE, useDataTimezone = TRUE)
  } else if (input == 2) {
    dg %>%
      dyOptions(includeZero = TRUE, labelsUTC = TRUE)
  } else if (input == 3) {
    dg %>%
      dyOptions(includeZero = TRUE)
  }
}

# Get first date in current year data table with date column named date
get_first_date = function(dt) {
  dt = copy(dt)
  dt[, year := year(date)]
  setorder(dt, "date")
  first_date = dt[year == year(Sys.Date()), .(date)][1, date]
  return(first_date)
}

# Calculate portfolio statistics
get_portfolio_stats = function(returns, clean = TRUE) {
  # Debug
  # returns = as.data.table(na.omit(Return.calculate(r)))
  # returns = rbi_lv[, .(date, strategy_ret, benchmark_ret)]

  # Portfolio statistics
  returns_xts = as.xts.data.table(returns)[, 1:2]

  # First date
  first_date = as.matrix(returns[, first(.SD), .SDcols = 1], rownames = NULL)
  first_date = data.table(Strategy = first_date[[1]],
                          Benchmark = first_date[[1]])
  rownames(first_date) = "Start Date"

  # Performance analytics
  ret_cum = Return.cumulative(returns_xts)
  ret_ann = Return.annualized(returns_xts)
  sr      = SharpeRatio(returns_xts)
  asr     = AdjustedSharpeRatio(returns_xts)

  # Downside frequency
  # PerformanceAnalytics::DownsideFrequency(returns_xts)

  # DDs
  dds = lapply(returns_xts, function(r) sortDrawdowns(findDrawdowns(r)))
  dd_max_loss = t(as.matrix(sapply(dds, function(x) min(x$return))))
  rownames(dd_max_loss) = "Max Drawdown"
  dd_max_length = t(as.matrix(sapply(dds, function(x) max(x$length))))
  rownames(dd_max_length) = "Max Drawdown Length"

  # No rownames
  if (any(grepl("ret", names(returns_xts)))) {
    beta = BetaCoVariance(returns_xts[, "strategy_ret"],
                          returns_xts[, "benchmark_ret"])
    beta = matrix(c(beta, 1), ncol = 2, dimnames = list("Beta", c("strategy_ret", "benchmark_ret")))
  } else {
    beta = BetaCoVariance(returns_xts[, "Strategy"],
                          returns_xts[, "Benchmark"])
    beta = matrix(c(beta, 1), ncol = 2, dimnames = list("Beta", c("Strategy", "Benchmark")))
  }

  # Combine
  portfolio_stats = rbind(ret_cum, ret_ann, sr, asr,
                          dd_max_loss, dd_max_length,
                          beta)
  portfolio_stats = apply(portfolio_stats, 2, round, 4)
  # PerformanceAnalytics::MartinRatio(returns_xts)

  return(portfolio_stats)
}

# Calculate portfolio statistics from strategy object
get_portfolio_stats_from_strategy = function(strategy, start_date = NULL, unit = NULL) {
  nav_units_ = strategy$calculate_nav_units(
    "SPY",
    unit = unit,
    start_date = start_date,
    end_date = NULL
  )
  nav_units_ = nav_units_[, .(date, Strategy, Benchmark)]
  r_ = na.omit(Return.calculate(as.xts.data.table(nav_units_)))
  get_portfolio_stats(as.data.table(r_))
}

# Init strategy
strategy = Strategy$new(lapply(FLEX_PRA, read_xml, options = "HUGE"), start_date = pra_start)
nav_units_ = strategy$calculate_nav_units("SPY", unit = NULL)
r = as.xts.data.table(nav_units_[, .(date, Strategy, Benchmark)])
na.omit(Return.calculate(r))
portfolio_stats = get_portfolio_stats(as.data.table(na.omit(Return.calculate(r))))


# DEBUG -------------------------------------------------------------------
# Init strategy
# strategy = Strategy$new(lapply(FLEX_EXUBER, read_xml), start_date = exuber_start)
# nav_units_ = strategy$calculate_nav_units("SPY", unit = NULL)
# r = as.xts.data.table(nav_units_[, .(date, Strategy, Benchmark)])
# na.omit(Return.calculate(r))
# portfolio_stats = get_portfolio_stats(as.data.table(na.omit(Return.calculate(r))))

# strategy = strategy_pra$clone()
# nav_units = strategy$calculate_nav_units("SPY", unit = NULL)
# nav_units = unique(nav_units, by = "date")
# # TODO duplicates
# r = as.xts.data.table(nav_units[, .(date, Strategy)])
# rpy = as.data.frame(as.data.table(r))
# colnames(rpy) = c("date", "Close")s
# py_run_string("
# dt = r.rpy
# dt['date'] = pd.to_datetime(dt['date'])
# series = pd.Series(dt['Close'].values, index=dt['date'])
# qs.reports.html(series, 'SPY', output='reports/report.html')
# ")
# REPORT
# yaml_content = list(
#   xml_paths = FLEX_PRA, # paste0(FLEX_PRA, collapse = ";"),
#   start_date = as.character(pra_start),
#   benchmark = "SPY"
# )
# yaml_content = yaml::as.yaml(yaml_content)
# writeLines(yaml_content, "params.yml")
# file_name_ = "report.html"
# render_command = paste(
#   'quarto render report.qmd --execute-params params.yml',
#   '--output ', file_name_,
#   '--output-dir ./reports'
# )
# system(render_command, wait = TRUE)

# ARCHIVE -----------------------------------------------------------------
# python environment
# if (Sys.info()[['user']] == 'Mislav') {
#   reticulate::use_virtualenv("C:/Users/Mislav/projects_py/pyquant", required = TRUE)
# } else {
#   reticulate::use_virtualenv("/opt/venv")
#   pd = reticulate::import("pandas")
#   qs = reticulate::import("quantstats")
# }
# # py_run_string('import pandas as pd; import quantstats as qs;')
# print("OVER")
#
# output$download_report = downloadHandler(
#   filename = function() {
#     paste0("report-", id, "-", cash_report_currency()[, last(acctAlias)], ".html")
#   },
#   content = function(file) {
#     req(returns())
#     showModal(modalDialog("Loading", footer=NULL))
#     on.exit(removeModal())
#     rpy = as.data.frame(unique(as.data.table(returns())), by = date)
#     colnames(rpy) = c("date", "Close")
#     assign("rpy", rpy, envir = .GlobalEnv)  # Assign rpy to global environment
#
#     py_run_string("
# import pandas as pd
# import quantstats as qs
# dt = r.rpy
# dt['date'] = pd.to_datetime(dt['date'])
# series = pd.Series(dt['Close'].values, index=dt['date'])
# qs.reports.html(series, 'SPY', output='reports/report.html')")
#
#     # Copy the temporary report to the file argument provided by Shiny
#     file.copy("reports/report.html", file, overwrite = TRUE)
#   }
# )
# })
