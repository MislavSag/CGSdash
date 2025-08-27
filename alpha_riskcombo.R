# source("imports.R")

# Trades summary
strategy_riskcombo = Strategy$new(lapply(FLEX_RISKCOMBO, read_xml, options = "HUGE"), riskcombo_start)
trades_dt_riskcombo_summary = strategy_riskcombo$summary_cfd_trades()

# UI
alpha_ui_riskcombo = function(ns) {
  tagList(
    card(
      card_header("riskcombo indicators graph"),
      max_height = "1000px",
      layout_column_wrap(
        layout_sidebar(
          sidebar = sidebar(
            selectInput(ns("select_tz_riskcombo"), label = h5("Select Timezone"),
                        choices = list("NY" = 1, "UTC" = 2, "Local" = 3),
                        selected = 1),
          ),
          # dygraphOutput(ns("indicators_riskcombo_graph")),
          br(),
          dataTableOutput(ns("indicators_riskcombo_table"))
        ),
      )

    )
  )
}

# Server
alpha_server_riskcombo = function(input, output, session, strategy) {

  output$indicators_riskcombo_table = renderDataTable({
    indicators_riskcombo_ = copy(indicators_risk_combo)
    setnames(indicators_riskcombo_, "time", "time_UTC")
    myDT(indicators_riskcombo_, paste0("indicators_riskcombo_", "-", Sys.Date()))
  }, server = FALSE)

  # output$indicators_riskcombo_graph <- renderDygraph({
  #   ind_trades = merge_indicators_trades(
  #     indicators_riskcombo[, .(time, riskcombo_indicator)],
  #     trades_dt_riskcombo_summary,
  #     "riskcombo_indicator"
  #   )
  #   ind_trades = ind_trades[, .(time_indicator, riskcombo_indicator, buy, sell)]
  #   ind_trades[, threshold := 5]
  #   dg = dygraph(as.xts.data.table(ind_trades), main = "riskcombo indicator") %>%
  #     # dyLimit(limit = 5, color = "red") %>%
  #     dySeries("buy", pointSize = 6, pointShape = "dot", color = "green") %>%
  #     dySeries("sell", pointSize = 6, pointShape = "dot", color = "red")
  #   set_timezone(dg, input = input$select_tz_riskcombo)
  # })
}
