# source("imports.R")

# Trades summary
strategy_pra = Strategy$new(lapply(FLEX_PRA, read_xml), pra_start)
trades_dt_pra_summary = strategy_pra$summary_cfd_trades()

# UI
alpha_ui_pra = function(ns) {
  tagList(
    card(
      card_header("PRA indicators graph"),
      max_height = "1000px",
      layout_column_wrap(
        layout_sidebar(
          sidebar = sidebar(
            selectInput(ns("select_tz_pra"), label = h5("Select Timezone"),
                        choices = list("NY" = 1, "UTC" = 2, "Local" = 3),
                        selected = 1),
          ),
          dygraphOutput(ns("indicators_pra_graph")),
          br(),
          dataTableOutput(ns("indicators_pra_table"))
        ),
      )

    )
  )
}

# Server
alpha_server_pra = function(input, output, session, strategy) {

  output$indicators_pra_table = renderDataTable({
    indicators_pra_ = copy(indicators_pra)
    setnames(indicators_pra_, "time", "time_UTC")
    myDT(indicators_pra_, paste0("indicators_pra_", "-", Sys.Date()))
  }, server = FALSE)

  output$indicators_pra_graph <- renderDygraph({
    ind_trades = merge_indicators_trades(
      indicators_pra[, .(time, pra_indicator)],
      trades_dt_pra_summary,
      "pra_indicator"
    )
    ind_trades = ind_trades[, .(time_indicator, pra_indicator, buy, sell)]
    ind_trades[, threshold := 5]
    dg = dygraph(as.xts.data.table(ind_trades), main = "PRA indicator") %>%
      # dyLimit(limit = 5, color = "red") %>%
      dySeries("buy", pointSize = 6, pointShape = "dot", color = "green") %>%
      dySeries("sell", pointSize = 6, pointShape = "dot", color = "red")
    set_timezone(dg, input = input$select_tz_pra)
  })
}
