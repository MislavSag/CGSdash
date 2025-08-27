# source("imports.R")

# Trades summary
strategy_alphapicks = Strategy$new(lapply(FLEX_ALPHAPICKS, read_xml, options = "HUGE"), alphapicks_start)
trades_dt_alphapicks_summary = strategy_alphapicks$summary_cfd_trades()

# UI
alpha_ui_alphapicks = function(ns) {
  tagList(
    card(
      card_header("alphapicks indicators graph"),
      max_height = "1000px",
      layout_column_wrap(
        layout_sidebar(
          sidebar = sidebar(
            selectInput(ns("select_tz_alphapicks"), label = h5("Select Timezone"),
                        choices = list("NY" = 1, "UTC" = 2, "Local" = 3),
                        selected = 1),
          ),
          # dygraphOutput(ns("indicators_riskcombo_graph")),
          br(),
          dataTableOutput(ns("indicators_alphapicks_table"))
        ),
      )

    )
  )
}

# Server
alpha_server_alphapicks = function(input, output, session, strategy) {

  output$indicators_alphapicks_table = renderDataTable({
    indicators_alphapicks_ = copy(indicators_alphapicks)
    setnames(indicators_alphapicks_, "time", "time_UTC")
    myDT(indicators_alphapicks_, paste0("indicators_alphapicks_", "-", Sys.Date()))
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
