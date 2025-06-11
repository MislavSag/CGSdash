source("imports.R")
source("module_strategy.R")
source("module_strategy_rbi.R")
source("alpha_pra.R")
source("alpha_minmax.R")
source("alpha_exuber.R")
source("alpha_exuber_old.R")
source("alpha_riskcombo.R")
source("alpha_alphapicks.R")

ui = bslib::page_navbar(
  header = autoWaiter(),
  title = "CGS Delta",
  id = "nav",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  sidebar = NULL,
  bslib::nav_panel("Overview", layout_columns(
    col_widths = c(-2, 8, -2),
    card(
      card_header("Strategy Performance"),
      max_height = "100%",
      layout_sidebar(sidebar = sidebar(
        checkboxInput("leverage", "Leverage", value = TRUE)
      ), DT::DTOutput("dt_shared_returns"))
    ),
    card(
      card_header("Positions"),
      max_height = "100%",
      uiOutput("ui_strategies_in_positions")
    )
  )),
  bslib::nav_panel("MinMax", module_strategy_ui("minmax", alpha_ui_minmax, "https://snpmarketdata.blob.core.windows.net/descriptions/MnMax.html")),
  bslib::nav_panel("PRA", module_strategy_ui("pra", alpha_ui_pra, "https://snpmarketdata.blob.core.windows.net/descriptions/PRAQC.html")),
  bslib::nav_panel("RiskCombo", module_strategy_ui("riskcombo", alpha_ui_riskcombo, "https://snpmarketdata.blob.core.windows.net/descriptions/RiskComboV2.html")), # alpha_ui_riskcombo
  bslib::nav_panel("AlphaPicks", module_strategy_ui("alphapicks", alpha_ui_alphapicks, NULL)),
  bslib::nav_panel("Exuber", module_strategy_ui("exuber", alpha_ui_exuber, "https://snpmarketdata.blob.core.windows.net/descriptions/ExuberBondsML.html")),
  bslib::nav_panel("Exuber old", module_strategy_ui("exuber_old", alpha_ui_exuber_old)),
  bslib::nav_panel("Exuber total", module_strategy_ui("exuber_total")),
  bslib::nav_panel("Least Volatile", module_strategy_ui_rbi("least_volatile")),
  # make dropdown list of nav_panels
  # nav_menu(
  #   title = "Strategies",
  #   nav_panel("Exuber old", module_strategy_ui("exuber_old", alpha_ui_exuber_old)),
  # )
)

server = function(input, output) {
  o_pra = module_strategy_server(
    "pra",
    FLEX_PRA,
    start_date = pra_start,
    end_date = NULL,
    alpha = alpha_server_pra
  )
  o_minmax = module_strategy_server(
    "minmax",
    FLEX_MINMAX,
    start_date = minmax_start,
    end_date = NULL,
    alpha = alpha_server_minmax
  )
  o_riskcombo = module_strategy_server(
    "riskcombo",
    FLEX_RISKCOMBO,
    start_date = riskcombo_start,
    end_date = NULL,
    alpha = alpha_server_riskcombo # alpha_server_riskcombo
  )
  o_alphapicks = module_strategy_server(
    "alphapicks",
    FLEX_ALPHAPICKS,
    start_date = alphapicks_start,
    end_date = NULL,
    alpha = alpha_server_alphapicks # alpha_server_riskcombo
  )
  o_exuber = module_strategy_server(
    "exuber",
    FLEX_EXUBER,
    start_date = exuber_start,
    end_date = NULL,
    alpha = alpha_server_exuber
  )
  portfolio_stats_exuber_old = module_strategy_server(
    "exuber_old",
    FLEX_EXUBER,
    start_date = exuber_old_start,
    end_date = exuber_start,
    alpha = alpha_server_exuber_old
  )
  portfolio_stats_exuber_total = module_strategy_server(
    "exuber_total",
    FLEX_EXUBER,
    start_date = exuber_start_total,
    end_date = NULL
  )
  o_lv = module_strategy_server_rbi(
    "least_volatile",
    nav_units = rbi_lv,
    positions = rbi_lv_portfolio,
    start_date = least_volatile_start,
    end_date = NULL,
    alpha = NULL
  )
  output$dt_shared_returns = DT::renderDT({
    leverage_ = if (isTRUE(input$leverage)) NULL else 2.66
    dates = c(o_pra()$date,
              o_minmax()$date,
              o_riskcombo()$date,
              o_exuber()$date,
              o_lv()$date) # , as.Date("2020-01-01")
    # print(head(Return.calculate(as.xts.data.table(o_pra()$nav_units[, .(date, Strategy, Benchmark)]))))

    # strategy()$calculate_nav_units(
    #   "SPY",
    #   unit = if (input$leverage == TRUE) NULL else 2,
    #   start_date = input$date_range[1],
    #   end_date = input$date_range[2]
    # )
    # if (input$leverage == TRUE) NULL else 2
    dt_ = cbind(
      get_portfolio_stats_from_strategy(o_pra()$strategy, o_pra()$date, leverage_),
      get_portfolio_stats_from_strategy(o_minmax()$strategy, o_minmax()$date, leverage_),
      get_portfolio_stats_from_strategy(o_riskcombo()$strategy, o_riskcombo()$date, leverage_),
      get_portfolio_stats_from_strategy(o_alphapicks()$strategy, o_alphapicks()$date, leverage_),
      get_portfolio_stats_from_strategy(o_exuber()$strategy, o_exuber()$date, leverage_),
      get_portfolio_stats(rbi_lv)
    )
    # colnames(dt_) = c("PRA", "Benchmark",
    #                   "MinMax", "Benchmark",
    #                   "Exuber", "Benchmark")
    # c(dates, o_lv()$date)
    dt_portfolio(dt_, "strategy_performance", dates) |>
      formatStyle(columns = 2:3, backgroundColor = "#f0f8ff") |>
      formatStyle(columns = 4:5, backgroundColor = "#f0fff0") |>
      formatStyle(columns = 6:7, backgroundColor = "#ffe4e1") |>
      formatStyle(columns = 8:9, backgroundColor = "#f0f8ff") |>
      formatStyle(columns = 10:11, backgroundColor = "#ffe4e1") |>
      formatStyle(columns = 12:13, backgroundColor = "#f0fff0")
  })

  output$ui_strategies_in_positions <- renderUI({
    # Check each module's `portfolio_has_asset`
    has_pra    = o_pra()$portfolio_has_asset
    has_minmax = o_minmax()$portfolio_has_asset
    has_riskcombo = o_riskcombo()$portfolio_has_asset
    has_alphapicks = o_alphapicks()$portfolio_has_asset
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
      "RiskCombo"   = has_riskcombo,
      "AlphaPicks"   = has_alphapicks
    )

    tagList(
      # tags$p("All strategies:"),
      tags$div(
        lapply(names(strategy_status), function(strat) {
          in_position <- strategy_status[[strat]]
          if (in_position %notin% c("FALSE", "TLT")) {
            # GREEN BADGE
            tags$span(
              class = "badge bg-success me-1",
              bs_icon("check-circle-fill", class = "text-white me-1", style = "font-size: 0.85em;"),
              strat
            )
          } else if (in_position == "TLT") {
            # BLUE BADGE
            tags$span(
              class = "badge bg-primary me-1",
              bs_icon("info-circle-fill", class = "text-white me-1", style = "font-size: 0.85em;"),
              strat
            )

          } else {
            # GRAY BADGE
            tags$span(
              class = "badge bg-secondary me-1",
              strat
            )
          }
        })
      )
    )
  })
}

shinyApp(ui, server)

# get_portfolio_stats_from_strategy = function(strategy, start_date = NULL, unit = NULL) {
#   nav_units_ = strategy$calculate_nav_units(
#     "SPY",
#     unit = unit,
#     start_date = start_date,
#     end_date = NULL
#   )
#   nav_units_ = nav_units_[, .(date, Strategy, Benchmark)]
#   r_ = na.omit(Return.calculate(as.xts.data.table(nav_units_)))
#   get_portfolio_stats(as.data.table(r_))
# }
#
