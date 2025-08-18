module_strategy_ui <- function(id, alpha = NULL, description = NULL) {
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
                             title = "Sharpe Ratio Annualized",
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
                                 checkboxInput(ns("leverage"), "Leverage", value = TRUE)
                               ),
                               dygraphOutput(ns("dygraph_performance"))
                             )
                           ),
                           card(
                             card_header("Portfolio summary"),
                             DT::DTOutput(ns("dt_portfolio_summary")))
                         )
                       )),
      if (!is.null(alpha)) {
        nav_panel(title = "Alpha", alpha(ns))
      },
      bslib::nav_panel(
        title = "Portfolio",
        card(
          card_header("Portfolio"),
          DT::DTOutput(ns("dt_open_positions"))
        )
      ),
      bslib::nav_panel(title = "Costs",
                       card(
                         fullRow = TRUE,
                         card_header("CFD costs"),
                         layout_sidebar(
                           sidebar = sidebar(
                             selectInput(ns("select_cfd_costs"),
                                         "CFD costs",
                                         choices = c("Cumulative", "Daily"),
                                         selected = "Cumulative"),
                             checkboxInput(ns("show_zeros"),
                                           "Show zeros",
                                           value = FALSE)
                           ),
                           dygraphOutput(ns("dygraph_cfd_costs")),
                         )
                       ),
      ),
      bslib::nav_panel(title = "Raw",
                       card(
                         card_header("Raw data"),

                         p("Click on below buttom to download raw Interactive Brokers Flex reports data"),
                         downloadButton(ns("download_tades"), "Trades"),
                         downloadButton(ns("download_cash_report_currency"), "Cash Report Curency"),
                         downloadButton(ns("download_cfd_charge"), "CFD Charge"),
                         downloadButton(ns("download_equity_summary_by_report_date_in_base"),
                                        "Equity Summary By Report Date In Base"),
                         downloadButton(ns("download_prior_period_position"), "Prior period position")
                       )
      ),
      if (!is.null(description)) {
        bslib::nav_panel(title = "Description",
                         card(
                           tags$iframe(
                             src = description,
                             width = "100%",
                             height = "800px",
                             frameborder = "0",
                             scrolling = "auto"
                           )
                         )
        )
      },
      bslib::nav_spacer(),
      # add downloadabe html report
      bslib::nav_item(
        tabName = "download",
        downloadButton(ns("download_btn"), "Preuzmi dokument") # , class = "btn btn-success"
        # downloadBttn(
        #   outputId = ns("download_report"),
        #   style = "bordered",
        #   color = "primary"
        # )
      )
    )
  )
}

module_strategy_server = function(id, xml_paths, start_date, end_date = NULL,
                                  alpha = NULL, description = NULL) {
  # Debug
  # xml_paths = FLEX_RISKCOMBO
  # start_date = riskcombo_start
  # end_date = NULL
  # strategy = Strategy$new(lapply(xml_paths, read_xml), start_date, end_date)
  # strategy$start_date
  # strategy$end_date
  # nav_units = strategy$calculate_nav_units("SPY", unit = NULL)
  # cfd_charge = strategy$extract_node("CFDCharge")
  # equity_summary_by_report_date_in_base = strategy$extract_node("EquitySummaryByReportDateInBase")
  # equity_summary_by_report_date_in_base[, format(last(total), big.mark = ",", scientific = FALSE)]
  # strategy$calculate_nav_units("SPY", unit = 2)
  # r = as.xts.data.table(nav_units[, .(date, Strategy, Benchmark)])
  # returns = na.omit(Return.calculate(r))
  # paste0(round(Return.annualized(returns)[1,1] * 100, 2), "%")
  # portfolio_stats = get_portfolio_stats(as.data.table(returns))
  # tryCatch({strategy()$extract_node("OpenPosition")}, error = function(e) NULL)

  # Create strategy
  strategy = reactive({
    # strategy = Strategy$new(lapply(xml_paths, read_xml), start_date, end_date)
    Strategy$new(lapply(xml_paths, read_xml), start_date, end_date)
  })

  # Import data
  # xml_paths_ = FLEX_PRA
  cash_report_currency = reactive({
    # strategy$extract_node("CashReportCurrency", FALSE)
    strategy()$extract_node("CashReportCurrency", FALSE)
  })
  equity_summary_by_report_date_in_base = reactive({
    # equity_summary_by_report_date_in_base = strategy$extract_node("EquitySummaryByReportDateInBase")
    strategy()$extract_node("EquitySummaryByReportDateInBase")
  })
  trades = reactive({
    # strategy$extract_node("Trade")
    strategy()$extract_node("Trade")
  })
  cfd_charge = reactive({
    # cfd_charge = strategy$extract_node("CFDCharge")
    strategy()$extract_node("CFDCharge")
  })
  prior_period_position = reactive({
    # strategy$extract_node("PriorPeriodPosition")
    strategy()$extract_node("PriorPeriodPosition")
  })
  open_positions = reactive({
    tryCatch({strategy()$extract_node("OpenPosition")}, error = function(e) NULL)
  })

  # Server
  moduleServer(id, function(input, output, session) {
    # SUMMARY -----------------------------------------------------------------
    output$nav = renderText({
      # equity_summary_by_report_date_in_base[, format(last(total), big.mark = ",", scientific = FALSE)]
      equity_summary_by_report_date_in_base()[, format(last(total), big.mark = ",", scientific = FALSE)]
    })
    output$start_date = renderText({
      # as.character(strategy$start_date)
      as.character(strategy()$start_date)
      format.Date(strategy()$start_date, "%d.%m.%Y")
    })
    output$return = renderText({
      # paste0(round(Return.cumulative(req(returns))[1,1] * 100, 2), "%")
      paste0(round(Return.cumulative(req(returns()))[1,1] * 100, 2), "%")
    })
    output$sharpe = renderText({
      round(SharpeRatio.annualized(req(returns()))[1,1], 2)
    })
    output$cagr = renderText({
      paste0(round(Return.annualized(req(returns()))[1,1] * 100, 2), "%")
    })

    nav_units = reactive({
      if (isTRUE(input$leverage)) {
        # nav_units_ = strategy$calculate_nav_units("SPY", unit = NULL)
        nav_units_ = strategy()$calculate_nav_units("SPY", unit = NULL)
      } else {
        nav_units_ = strategy()$calculate_nav_units("SPY", unit = 2.66)
      }
      print(nav_units_)
      nav_units_
    })

    observe({
      updateDateRangeInput(
        session,
        "date_range",
        start = nav_units()[, min(date)],
        min = nav_units()[, min(date)],
        end = nav_units()[, max(date)],
        max = nav_units()[, max(date)]
      )
    })
    observeEvent(input$ytd_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = get_first_date(nav_units()),
        min = get_first_date(nav_units()),
        end = Sys.Date(),
        max = Sys.Date()
      )
    })
    observeEvent(input$m1_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = max(Sys.Date() - 30, nav_units()[, min(date)]),
        min = max(Sys.Date() - 30, nav_units()[, min(date)]),
        end = Sys.Date(),
        max = Sys.Date()
      )
    })
    observeEvent(input$m6_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = max(Sys.Date() - 180, nav_units()[, min(date)]),
        min = max(Sys.Date() - 180, nav_units()[, min(date)]),
        end = Sys.Date(),
        max = Sys.Date()
      )
    })
    observeEvent(input$reset_button, {
      updateDateRangeInput(
        session,
        "date_range",
        start = nav_units()[, min(date)],
        min = nav_units()[, min(date)],
        end = nav_units()[, max(date)],
        max = nav_units()[, max(date)]
      )
    })

    nav_units_update = reactive({
      req(!is.null(input$date_range[1]) | !is.null(input$date_range[1]))
      if (input$date_range[1] == nav_units()[, min(date)] &
          input$date_range[2] == nav_units()[, max(date)]) {
        return(NULL)
      }
      # nav_units_update = strategy$calculate_nav_units(
      #   "SPY",
      #   unit = 2,
      #   start_date = as.Date(strategy$start_date),
      #   end_date = Sys.Date()
      # )
      print("UPDATE!!!!!!!!!!!!!!!!!!!!!!!")
      strategy()$calculate_nav_units(
        "SPY",
        unit = if (input$leverage == TRUE) NULL else 2.66,
        start_date = input$date_range[1],
        end_date = input$date_range[2]
      )
    })

    returns = reactive({
      req(nav_units())
      if (!is.null(nav_units_update())) {
        # r = as.xts.data.table(nav_units_update[, .(date, Strategy, Benchmark)])
        r = as.xts.data.table(nav_units_update()[, .(date, Strategy, Benchmark)])
      } else {
        r = as.xts.data.table(nav_units()[, .(date, Strategy, Benchmark)])
      }
      na.omit(Return.calculate(r))
    })

    portfolio_stats = reactive(get_portfolio_stats(as.data.table(returns())))

    output$dygraph_performance = renderDygraph({
      # dygraph(na.omit(nav_units[, .(date, Benchmark, Strategy, StrategyGross)])) |>
      #   dyCSS("dygraph.css")
      if (input$date_range[1] == nav_units()[, min(date)] &
          input$date_range[2] == nav_units()[, max(date)]) {
        dygraph(na.omit(nav_units()[, .(date, Benchmark, Strategy, StrategyGross)])) |>
          dyCSS("www/dygraph.css")
      } else {
        dygraph(na.omit(nav_units_update()[, .(date, Benchmark, Strategy, StrategyGross)])) |>
          dyCSS("www/dygraph.css")
      }
    })

    output$dt_portfolio_summary = DT::renderDT({
      dt_portfolio(portfolio_stats(), "portfolio_summary")
    })

    # ALPHA -------------------------------------------------------------------
    if (!is.null(alpha)) {
      alpha(input, output, session, strategy)
    }


    # PORTFOLIO ---------------------------------------------------------------
    output$dt_open_positions = DT::renderDT({
      if (!is.null(open_positions())) {
        portfolio = open_positions()
        portfolio[ , reportDate := as.Date(reportDate)]
        # Report date should be in current year
        portfolio = portfolio[reportDate > as.Date(paste0(year(Sys.Date()), "-01-01"))]
      } else {
        portfolio = data.table("No data" = "No data")
      }
      DT::datatable(portfolio)
    })
    # output$dt_open_positions = DT::renderDT({
    #   if (!is.null(open_positions())) {
    #     DT::datatable(open_positions())
    #   } else {
    #     DT::datatable(data.table(
    #       "No data" = "No data"
    #       )
    #     )
    #   }
    # })


    # COSTS -------------------------------------------------------------------
    output$dygraph_cfd_costs = renderDygraph({
      # cfd_charge = strategy$extract_node("CFDCharge")
      req(input$select_cfd_costs)

      # Default is without zeroes, otherwise with zeroes
      if (input$show_zeros == TRUE) {
        cfd_charge_ = cfd_charge()[, .(date, total)][
          equity_summary_by_report_date_in_base()[, .(date = reportDate, nav = total)],
          on = "date"]
        setnafill(cfd_charge_, cols = "total", fill = 0)
      } else {
        cfd_charge_ = cfd_charge()
      }

      if (input$select_cfd_costs == "Cumulative") {
        dygraph(cfd_charge_[, .(date, `CFD costs` = cumsum(-total))])
      } else {
        dygraph(cfd_charge_[, .(date, `CFD costs` = -total)])
      }
    })

    # DOWNLOAD -----------------------------------------------------
    output$download_tades = downloadHandler(
      filename = function() {
        "trades.csv"
      },
      content = function(file) {
        write.csv(trades(), file)
      }
    )
    output$download_cash_report_currency = downloadHandler(
      filename = function() {
        "cash_report_currency.csv"
      },
      content = function(file) {
        write.csv(cash_report_currency(), file)
      }
    )
    output$download_equity_summary_by_report_date_in_base = downloadHandler(
      filename = function() {
        "equity_summary_by_report_date_in_base.csv"
      },
      content = function(file) {
        write.csv(equity_summary_by_report_date_in_base(), file)
      }
    )
    output$download_cfd_charge = downloadHandler(
      filename = function() {
        "cfd_charge.csv"
      },
      content = function(file) {
        write.csv(cfd_charge(), file)
      }
    )
    output$download_prior_period_position = downloadHandler(
      filename = function() {
        "prior_period_position.csv"
      },
      content = function(file) {
        write.csv(prior_period_position(), file)
      }
    )

    output$download_btn = downloadHandler(
      filename = function() {
        paste0("report-", cash_report_currency()[, last(acctAlias)], ".html")
        # "report.html"
      },
      content = function(file) {
        # xml_paths_ = FLEX_PRA
        # start_date = pra_start
        # yaml_content = list(
        #   xml_paths = paste0(xml_paths_, collapse = ";"),
        #   start_date = as.character(start_date),
        #   benchmark = "SPY"
        # )
        # yaml_content = yaml::as.yaml(yaml_content)
        # writeLines(yaml_content, "params.yml")
        # temp_file = tempfile(fileext = ".html")
        #
        # quarto::quarto_render("report.qmd",
        #                       execute_params = list(xml_paths = paste0(xml_paths_, collapse = ";"),
        #                                             start_date = as.character(start_date),
        #                                             benchmark = "SPY"),
        #                       output_file = temp_file)
        #
        #
        # render_command = paste(
        #   'quarto render report.qmd --execute-params params.yml',
        #   '--output ', temp_file
        # )
        # system(render_command, wait = TRUE)

        # temp_file = tempfile(fileext = ".html")

        # # Show notification that report is generating
        # id <- showNotification(
        #   "Rendering report...",
        #   duration = NULL,
        #   closeButton = FALSE
        # )
        # on.exit(removeNotification(id), add = TRUE)**

        # show modal
        showModal(modalDialog(
          title = "Rendering report",
          "Please wait while the report is being generated...",
          footer = NULL
        ))

        rmarkdown::render(
          input="report.Rmd",
          params = list(xml_paths = paste0(xml_paths, collapse = ";"),
                        start_date = as.character(start_date),
                        benchmark = "SPY"),
          output_file = file,
          envir = new.env(parent = globalenv())
        )

        # render_command <- paste('quarto render forensis_quarto.qmd --execute-params', param_file,
        #                         '--output ', file_name_,
        #                         '--output-dir reports')
        # system(render_command, wait = TRUE)

        removeModal()

        # file.copy(temp_file, file)
      }
    )

    reactive({
      print(id)
      print(open_positions())
      open_positions = !is.null(open_positions()) &&
        nrow(open_positions()[ , reportDate := as.Date(reportDate)][
          reportDate > as.Date(paste0(year(Sys.Date()), "-01-01"))]) > 0
      if (open_positions == TRUE) {
        open_positions = open_positions()[1, symbol]
      } else {
        open_positions = FALSE
      }
      list(strategy = strategy(), date = strategy()$start_date,
           portfolio_has_asset = open_positions)
    })

  })
}
