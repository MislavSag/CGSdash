# source("imports.R")

# Trades summary
strategy_exuber = Strategy$new(lapply(FLEX_EXUBER, read_xml), exuber_start)
trades_dt_exuber_summary = strategy_exuber$summary_cfd_trades()

# UI
alpha_ui_exuber = function(ns) {
  tagList(
    card(
      card_header("exuber indicators graph"),
      max_height = "1000px",
      layout_column_wrap(
        layout_sidebar(
          sidebar = sidebar(
            selectInput(ns("select_tz_exuber"), label = h5("Select Timezone"),
                        choices = list("NY" = 1, "UTC" = 2, "Local" = 3),
                        selected = 1),
          ),
          dygraphOutput(ns("indicators_exuber_graph")),
          br(),
          dataTableOutput(ns("indicators_exuber_table"))
        ),
      )

    )
  )
}

# Server
alpha_server_exuber = function(input, output, session, strategy) {

  output$indicators_exuber_table = renderDataTable({
    indicators_exuber_ = indicators_exuber[time > exuber_start]
    setnames(indicators_exuber_, "time", "time_UTC")
    myDT(indicators_exuber_, paste0("indicators_exuber_", "-", Sys.Date()))
  }, server = FALSE)

  output$indicators_exuber_graph = renderDygraph({
    ind_trades = merge_indicators_trades(
      indicators_exuber[time > exuber_start, .(time,
                            RADF,
                            tar_threshold_1,
                            tar_threshold_2,
                            radf_best_param,
                            wf_threshold)],
      trades_dt_exuber_summary, "RADF"
    )
    ind_trades = ind_trades[, .(time_indicator, RADF,
                                radf_best_param,
                                wf_threshold, buy, sell)]
    dg = dygraph(as.xts.data.table(ind_trades), main = "Exuber indicator") %>%
      dySeries("buy", pointSize = 6, pointShape = "dot", color = "green") %>%
      dySeries("sell", pointSize = 6, pointShape = "dot", color = "red")
    set_timezone(dg, input = input$select_tz_exuber)
  })
}
