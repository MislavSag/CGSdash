# source("imports.R")

# Trades summary
strategy_vixcombo = Strategy$new(lapply(FLEX_VIXCOMBO, read_xml, options = "HUGE"), vixcombo_start)
trades_dt_vixcombo_summary = strategy_vixcombo$summary_cfd_trades()

# UI
alpha_ui_vixcombo = function(ns) {
  tagList(
    card(
      card_header("vixcombo indicators graph"),
      max_height = "1000px",
      layout_column_wrap(
        layout_sidebar(
          sidebar = sidebar(
            selectInput(ns("select_tz_vixcombo"), label = h5("Select Timezone"),
                        choices = list("NY" = 1, "UTC" = 2, "Local" = 3),
                        selected = 1),
          ),
          # dygraphOutput(ns("indicators_riskcombo_graph")),
          br(),
          dataTableOutput(ns("indicators_vixcombo_table"))
        ),
      )

    )
  )
}

# Server
alpha_server_vixcombo = function(input, output, session, strategy) {

  output$indicators_vixcombo_table = renderDataTable({
    indicators_vixcombo_ = copy(indicators_vix_combo)
    setnames(indicators_vixcombo_, "time", "time_UTC")
    myDT(indicators_vixcombo_, paste0("indicators_vixcombo_", "-", Sys.Date()))
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
