# source("imports.R")

# Trades summary
strategy_minmax = Strategy$new(lapply(FLEX_MINMAX, read_xml), minmax_start)
trades_dt_minmax_summary = strategy_minmax$summary_cfd_trades()

# UI
alpha_ui_minmax = function(ns) {
  tagList(
    card(
      card_header("minmax indicators graph"),
      max_height = "1000px",
      layout_column_wrap(
        layout_sidebar(
          sidebar = sidebar(
            selectInput(ns("select_tz_minmax"), label = h5("Select Timezone"),
                        choices = list("NY" = 1, "UTC" = 2, "Local" = 3),
                        selected = 1),
          ),
          dygraphOutput(ns("indicators_minmax_graph")),
          br(),
          p("Note: Red shaded area refers to time when there was error
                   in the Quanconnect live node (without email notificaton):
                   Brokerage Info: Request Account Data Sending Error - .
                   We have developed Azure Function that checks for logs
                   every hour to see if strategy is live."),
          br(),
          dataTableOutput(ns("indicators_minmax_table"))
        ),
      )

    )
  )
}

# Server
alpha_server_minmax = function(input, output, session, strategy) {

  output$indicators_minmax_table = renderDataTable({
    indicators_minmax_ = copy(indicators_minmax)
    setnames(indicators_minmax_, "time", "time_UTC")
    myDT(indicators_minmax_, paste0("indicators_minmax_", "-", Sys.Date()))
  }, server = FALSE)

  output$indicators_minmax_graph <- renderDygraph({
    ind_trades = merge_indicators_trades(
      indicators_minmax[, .(time, minmax_indicator)],
      trades_dt_minmax_summary,
      "minmax_indicator"
    )
    ind_trades = ind_trades[, .(time_indicator, minmax_indicator, buy, sell)]
    ind_trades[, threshold := -0.004]
    dg = dygraph(as.xts.data.table(ind_trades), main = "MinMax indicator") %>%
      # dyLimit(limit = -0.004, color = "red") %>%
      dySeries("buy", pointSize = 6, pointShape = "dot", color = "green") %>%
      dySeries("sell", pointSize = 6, pointShape = "dot", color = "red") %>%
      dyShading(from = "2023-02-28", to = "2023-03-07", color = "#FFE6E6")
    set_timezone(dg, input = input$select_tz_minmax)
  })
}
