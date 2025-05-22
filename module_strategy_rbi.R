module_strategy_ui_rbi = function(id, alpha = NULL) {
  ns = NS(id)
  tagList(
    bslib::navset_tab(
      bslib::nav_panel(title = "Summary",
                       tagList(
                         layout_column_wrap(
                           width = "250px",
                           fill = FALSE,
                           value_box(
                             title = "NAV",
                             value = textOutput(ns("nav")),
                             showcase = bs_icon("chevron-double-right")
                           ),
                           value_box(
                             title = "Start date",
                             value = textOutput(ns("start_date")),
                             showcase = bs_icon("calendar")
                           ),
                           value_box(
                             title = "Return",
                             value = textOutput(ns("return")),
                             showcase = bs_icon("graph-up")
                           ),
                           value_box(
                             title = "Sharpe Ratio",
                             value = textOutput(ns("sharpe")),
                             showcase = bs_icon("arrows-collapse")
                           ),
                           value_box(
                             title = "CAGR",
                             value = textOutput(ns("cagr")),
                             showcase = bs_icon("graph-up")
                           )
                         ),
                         layout_columns(
                           col_widths = c(12, 6),
                           card(
                             card_header("Strategy performance"),
                             layout_sidebar(
                               sidebar = sidebar(
                                 dateRangeInput(ns("date_range"),
                                                "Date range",
                                                start = NULL,
                                                end = NULL,
                                                min = NULL,
                                                max = NULL,
                                                separator = "-",
                                                format = "dd.mm.yy",
                                 ),
                                 # Add year to date option for dates, as button
                                 fluidRow(
                                   column(width = 6, actionButton(ns("ytd_button"), "YTD", width = "100%", class = "btn-small-text")),
                                   column(width = 6, actionButton(ns("m1_button"), "1M", width = "100%", class = "btn-small-text"))
                                 ),
                                 fluidRow(
                                   column(width = 6, actionButton(ns("m6_button"), "6M", width = "100%", class = "btn-small-text")),
                                   column(width = 6, actionButton(ns("reset_button"), "ALL", width = "100%", class = "btn-small-text"))
                                 ),
                                 # numericInput(ns("scale_strategy"), "Scale strategy", value = 1, min = 1),
                                 # checkboxInput(ns("leverage"), "Leverage", value = TRUE)
                               ),
                               dygraphOutput(ns("dygraph_performance"))
                             )
                           ),
                           card(
                             card_header("Portfolio summary"),
                             DT::DTOutput(ns("dt_portfolio_summary")))
                         )
                       )),
      # if (!is.null(alpha)) {
      #   nav_panel(title = "Alpha", alpha(ns))
      # },
      bslib::nav_panel(
        title = "Portfolio",
        card(
          card_header("Portfolio"),
          DT::DTOutput(ns("dt_open_positions"))
        )
      ),
      # bslib::nav_panel(title = "Costs",
      #                  card(
      #                    fullRow = TRUE,
      #                    card_header("CFD costs"),
      #                    layout_sidebar(
      #                      sidebar = sidebar(
      #                        selectInput(ns("select_cfd_costs"),
      #                                    "CFD costs",
      #                                    choices = c("Cumulative", "Daily"),
      #                                    selected = "Cumulative"),
      #                        checkboxInput(ns("show_zeros"),
      #                                      "Show zeros",
      #                                      value = FALSE)
      #                      ),
      #                      dygraphOutput(ns("dygraph_cfd_costs")),
      #                    )
      #                  ),
      # ),
      bslib::nav_panel(title = "Raw",
                       card(
                         card_header("Raw data"),

                         p("Click on below buttom to download raw Interactive Brokers Flex reports data"),
                         downloadButton(ns("download_nav_units"), "NAV units"),
                         downloadButton(ns("download_positions"), "Positions"),
                       )
      ),
      # bslib::nav_spacer(),
      # # add downloadabe html report
      # bslib::nav_item(
      #   tabName = "download",
      #   downloadButton(ns("download_btn"), "Preuzmi dokument") # , class = "btn btn-success"
      #   # downloadBttn(
      #   #   outputId = ns("download_report"),
      #   #   style = "bordered",
      #   #   color = "primary"
      #   # )
      # )
    )
  )
}

module_strategy_server_rbi = function(id,
                                      nav_units,
                                      positions = NULL,
                                      start_date,
                                      end_date = NULL,
                                      alpha = NULL) {
  # Debug
  # nav_units = copy(rbi_lv)

  # Server
  moduleServer(id, function(input, output, session) {

    # SUMMARY -----------------------------------------------------------------
    output$nav = renderText({
      nav_units[, data.table::last(strategy)]
    })
    output$start_date = renderText({
      format.Date(start_date, "%d.%m.%Y")
    })
    output$return = renderText({
      paste0(round(Return.cumulative(nav_units[, strategy_ret]) * 100, 2), "%")
    })
    output$sharpe = renderText({
      round(SharpeRatio.annualized(req(nav_units[, .(date, strategy_ret)]))[1,1], 2)
    })
    output$cagr = renderText({
      paste0(round(Return.annualized(nav_units[, .(date, strategy_ret)])[1,1] * 100, 2), "%")
    })

    observe({
      updateDateRangeInput(
        session,
        "date_range",
        start = nav_units[, min(date)],
        min = nav_units[, min(date)],
        end = nav_units[, max(date)],
        max = nav_units[, max(date)]
      )
    })
    observeEvent(input$ytd_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = get_first_date(nav_units),
        min = get_first_date(nav_units),
        end = Sys.Date(),
        max = Sys.Date()
      )
    })
    observeEvent(input$m1_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = max(Sys.Date() - 30, nav_units[, min(date)]),
        min = max(Sys.Date() - 30, nav_units[, min(date)]),
        end = Sys.Date(),
        max = Sys.Date()
      )
    })
    observeEvent(input$m6_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = max(Sys.Date() - 180, nav_units[, min(date)]),
        min = max(Sys.Date() - 180, nav_units[, min(date)]),
        end = Sys.Date(),
        max = Sys.Date()
      )
    })
    observeEvent(input$reset_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = nav_units[, min(date)],
        min = nav_units[, min(date)],
        end = nav_units[, max(date)],
        max = nav_units[, max(date)]
      )
    })

    nav_units_update = reactive({
      req(!is.null(input$date_range[1]) | !is.null(input$date_range[1]))
      # if (input$date_range[1] == nav_units[, min(date)] &
      #     input$date_range[2] == nav_units[, max(date)]) {
      #   return(NULL)
      # }
      nav_units[date >= input$date_range[1] & date <= input$date_range[2] + days(1)]
    })

    # returns = reactive({
    #   req(nav_units())
    #   if (!is.null(nav_units_update())) {
    #     r = as.xts.data.table(nav_units_update()[, .(date, Strategy, Benchmark)])
    #   } else {
    #     r = as.xts.data.table(nav_units()[, .(date, Strategy, Benchmark)])
    #   }
    #   na.omit(Return.calculate(r))
    # })

    portfolio_stats = reactive({
      if (input$date_range[1] == nav_units[, min(date)] &
          input$date_range[2] == nav_units[, max(date)]) {
        dt_ = nav_units[, .(date, strategy_ret, benchmark_ret)]
      } else {
        dt_ = nav_units_update()[, .(date, strategy_ret, benchmark_ret)]
      }
      get_portfolio_stats(dt_)
    })

    output$dygraph_performance = renderDygraph({
      # dygraph(na.omit(nav_units[, .(date, Benchmark, Strategy, StrategyGross)])) |>
      #   dyCSS("dygraph.css")
      if (input$date_range[1] == nav_units[, min(date)] &
          input$date_range[2] == nav_units[, max(date)]) {
        dygraph(na.omit(nav_units[, .(date, benchmark, strategy)])) |>
          dyCSS("www/dygraph.css")
      } else {
        dygraph(na.omit(nav_units_update()[, .(date, benchmark, strategy)])) |>
          dyCSS("www/dygraph.css")
      }
    })

    output$dt_portfolio_summary = DT::renderDT({
      dt_portfolio(portfolio_stats(), "portfolio_summary")
    })
    #
    #   # ALPHA -------------------------------------------------------------------
    #   if (!is.null(alpha)) {
    #     alpha(input, output, session, strategy)
    #   }
    #
    #
    # PORTFOLIO ---------------------------------------------------------------
    output$dt_open_positions = DT::renderDT({
      if (!is.null(positions)) {
        portfolio = rbi_lv_portfolio
      } else {
        portfolio = data.table("No data" = "No data")
      }
      DT::datatable(portfolio)
    })

    #
    #   # COSTS -------------------------------------------------------------------
    #   output$dygraph_cfd_costs = renderDygraph({
    #     # cfd_charge = strategy$extract_node("CFDCharge")
    #     req(input$select_cfd_costs)
    #
    #     # Default is without zeroes, otherwise with zeroes
    #     if (input$show_zeros == TRUE) {
    #       cfd_charge_ = cfd_charge()[, .(date, total)][
    #         equity_summary_by_report_date_in_base()[, .(date = reportDate, nav = total)],
    #         on = "date"]
    #       setnafill(cfd_charge_, cols = "total", fill = 0)
    #     } else {
    #       cfd_charge_ = cfd_charge()
    #     }
    #
    #     if (input$select_cfd_costs == "Cumulative") {
    #       dygraph(cfd_charge_[, .(date, `CFD costs` = cumsum(-total))])
    #     } else {
    #       dygraph(cfd_charge_[, .(date, `CFD costs` = -total)])
    #     }
    #   })
    #
    # DOWNLOAD -----------------------------------------------------
    output$download_nav_units = downloadHandler(
      filename = function() {
        "nav_units.csv"
      },
      content = function(file) {
        write.csv(nav_units, file)
      }
    )
    output$download_positions = downloadHandler(
      filename = function() {
        "positions.csv"
      },
      content = function(file) {
        write.csv(positions, file)
      }
    )
    #
    #   output$download_btn = downloadHandler(
    #     filename = function() {
    #       paste0("report-", cash_report_currency()[, last(acctAlias)], ".html")
    #       # "report.html"
    #     },
    #     content = function(file) {
    #       # show modal
    #       showModal(modalDialog(
    #         title = "Rendering report",
    #         "Please wait while the report is being generated...",
    #         footer = NULL
    #       ))
    #
    #       rmarkdown::render(
    #         input="report.Rmd",
    #         params = list(xml_paths = paste0(xml_paths, collapse = ";"),
    #                       start_date = as.character(start_date),
    #                       benchmark = "SPY"),
    #         output_file = file,
    #         envir = new.env(parent = globalenv())
    #       )
    #
    #       removeModal()
    #     }
    #   )
    #
    reactive(list(date = start_date))
  })
}
