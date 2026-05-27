#' #' @rdname ui-panel-categories-comparator-two
#' #' @export
#' ui_panel_categories_comparator_two <- function(id) {
#'     ns <- shiny::NS(id)
#'     card_height      <- getOption("app_card_height_md")
#'     card_height_text <- getOption("app_card_height_sm")
#' 
#'     # Box Plot -----------------------------------------------------------------
#' 
#'     box_plot <- bslib::card(
#'         height      = card_height,
#'         full_screen = TRUE,
#' 
#'         bslib::card_header(
#'             bslib::card_title(
#'                 container = tags$h2,
#'                 class     = "my-2 fs-5",
#'                 shiny::textOutput(ns("box_plot_title"), tags$span)
#'             )
#'         ),
#' 
#'         bslib::card_body(
#'             shiny::plotOutput(ns("box_plot"))
#'         ),
#' 
#'         bslib::card_footer(
#'             shiny::uiOutput(ns("box_plot_desc"), container = tags$p)
#'         )
#'     )
#' 
#'     # Descriptive Statistics ---------------------------------------------------
#' 
#'     one <- bslib::card(
#'         height      = card_height,
#'         full_screen = TRUE,
#' 
#'         bslib::card_header(
#'             bslib::card_title(
#'                 container = tags$h2,
#'                 class     = "my-2 fs-5",
#'                 shiny::textOutput(ns("one_title"), tags$span)
#'             )
#'         ),
#' 
#'         bslib::card_header(
#'             shiny::numericInput(
#'                 inputId = ns("expected_ratio_dist_params"),
#'                 label   = "",
#'                 value   = 0.5,
#'                 min     = 0,
#'                 max     = 1
#'             )
#'         ),
#' 
#'         bslib::card_body(
#'             class = "px-5",
#'             shiny::uiOutput(ns("one"))
#'         ),
#' 
#'         bslib::card_footer(
#'             shiny::textOutput(ns("one_desc"), tags$p)
#'         )
#'     )
#' 
#'     # Panel --------------------------------------------------------------------
#' 
#'     ui <- bslib::nav_panel(
#'         value = id,
#'         title = shiny::textOutput(ns("title"), tags$span),
#' 
#'         bslib::layout_column_wrap(
#'             width         = 1/2,
#'             fill          = FALSE,
#'             heights_equal = "row",
#' 
#'             box_plot,
#'             one
#'         )
#'     )
#' 
#'     return(ui)
#' }
#' 
#' #' @rdname ui-panel-categories-comparator-two
#' #' @export
#' server_panel_categories_comparator_two <- function(
#'     id,
#'     lang,
#'     inputs_calc,
#'     data_sample,
#'     simulations)
#' {
#'     stopifnot(exprs = {
#'         shiny::is.reactive(lang)
#'         shiny::is.reactive(inputs_calc)
#'         shiny::is.reactive(data_sample)
#'         shiny::is.reactive(simulations)
#'     })
#' 
#'     server <- function(input, output, session) {
#'         title <- shiny::reactive({
#'             translate(lang = lang(), "Compare Two Categories")
#'         }) |>
#'         shiny::bindCache(lang())
#' 
#'         expected_ratio_dist_params_label <- shiny::reactive({
#'             translate(lang = lang(), "expected changes (as a ratio) for the GM and GSD")
#'         })
#' 
#'         output$title <- shiny::renderText({
#'             title()
#'         })
#' 
#'         output$box_plot_title <- shiny::renderText({
#'             translate(lang = lang(), "Box and Whisker Plot")
#'         }) |>
#'         shiny::bindCache(lang())
#' 
#'         output$one_title <- shiny::renderText({
#'             translate(lang = lang(), "
#'                 Expected Changes in Distribution Parameters
#'             ")
#'         }) |>
#'         shiny::bindCache(lang())
#' 
#'         output$box_plot <- renderPlot({
#'             lang <- lang()
#'             inputs_calc <- inputs_calc()
#' 
#'             boxplot.2.cat(
#'                 data.formatted    = data_sample(),
#'                 bayesian.output.D = simulations(),
#'                 cat1              = inputs_calc$data_chosen_category,
#'                 cat2              = inputs_calc$data_chosen_category_2,
#'                 boxplot.2cat.1    = inputs_calc$data_chosen_category,
#'                 boxplot.2cat.2    = inputs_calc$data_chosen_category_2,
#'                 boxplot.2cat.3    = translate(lang = lang, "Concentration"),
#'                 boxplot.2cat.4    = translate(lang = lang, "OEL"),
#'                 boxplot.2cat.5    = translate(lang = lang, "Category")
#'             )
#'         })
#' 
#'         output$box_plot_desc <- shiny::renderUI({
#'             lang <- lang()
#' 
#'             html(
#'                 translate(lang = lang, "
#'                     Each category has its own box and whisker plot. The
#'                     measurements are scattered around the around the midpoint
#'                     of the x-axis. The box (outer horizontal lines) represents
#'                     the distance between the %s and %s percentiles. The whisker
#'                     (vertical line) represent the distance between the %s and
#'                     %s percentiles. The inner black horizontal line is the
#'                     median. The OEL is shown as a red line. Shaded and colored
#'                     points represent the idealized distribution, while bold
#'                     points represent the actual observations. See Frequently
#'                     Asked Questions for more information on how non-detects
#'                     (censored values) are imputed.
#'                 "),
#'                 ordinal(25L, lang),
#'                 ordinal(75L, lang),
#'                 ordinal(10L, lang),
#'                 ordinal(90L, lang)
#'             )
#'         }) |>
#'         shiny::bindCache(lang())
#'         
#'         # updated code 
#'         
#'         output$one <- shiny::renderUI({
#'           lang <- lang()
#'           inputs_calc <- inputs_calc()
#' 
#'           user.input <- c(
#'             inputs_calc,
#'             list(exp.ratio.gm = input$expected_ratio_dist_params)
#'           )
#'           
#'           tbl <- fun.2cat.dist(
#'             bayesian.output.D = simulations(),
#'             cat1              = inputs_calc$data_chosen_category,
#'             cat2              = inputs_calc$data_chosen_category_2,
#'             user.input        = user.input,
#'             comp.2cat.1       = translate(lang = lang, "Geometric Mean"),
#'             comp.2cat.2       = translate(lang = lang, "Geometric Standard Deviation"),
#'             comp.2cat.3       = translate(lang = lang, "Parameter"),
#'             comp.2cat.4       = translate(lang = lang, "Category")
#'           )
#'           
#'           as_html_table(tbl)
#'         })
#'         
#'         # Previous code 
#'         
#'         #output$one <- shiny::renderUI({
#'         #    lang <- lang()
#'         #    inputs_calc <- inputs_calc()
#' 
#'         #    tbl <- fun.2cat.dist(
#'         #        bayesian.output.D = simulations(),
#'         #        cat1              = inputs_calc$data_chosen_category,
#'         #        cat2              = inputs_calc$data_chosen_category_2,
#'         #        user.input        = inputs_calc,
#'         #        comp.2cat.1       = translate(lang = lang, "Geometric Mean"),
#'         #        comp.2cat.2       = translate(lang = lang, "Geometric Standard Deviation"),
#'         #        comp.2cat.3       = translate(lang = lang, "Parameter"),
#'         #        comp.2cat.4       = translate(lang = lang, "Category")
#'         #    )
#' 
#'             # Overwrite internal row names
#'             # (stored in the first column).
#'             # stats[[1]] <- stats_dim_names$rows
#' 
#'         #    as_html_table(tbl)
#'         #})
#' 
#'         #output$one_desc <- shiny::renderText({
#'         #    translate(lang = lang(), "
#'         #        Hello, world!
#'         #    ")
#'         #})
#' 
#'         # Translate elements not rendered
#'         # with a shiny::render*() function.
#'         shiny::observe({
#'             shiny::updateNumericInput(
#'                 inputId = "expected_ratio_dist_params",
#'                 label   = expected_ratio_dist_params_label()
#'             )
#'         })
#' 
#'         return(title)
#'     }
#' 
#'     return(shiny::moduleServer(id, server))
#' }



# new new code 


#' @rdname ui-panel-categories-comparator-two
#' @export
ui_panel_categories_comparator_two <- function(id) {
  ns <- shiny::NS(id)
  card_height      <- getOption("app_card_height_md")
  card_height_text <- getOption("app_card_height_sm")
  
  
  box_plot <- bslib::card(
    height      = card_height,
    full_screen = TRUE,
    
    bslib::card_header(
      bslib::card_title(
        container = tags$h2,
        class     = "my-2 fs-5",
        shiny::textOutput(ns("box_plot_title"), tags$span)
      )
    ),
    
    bslib::card_body(
      shiny::plotOutput(ns("box_plot"))
    ),
    
    bslib::card_footer(
      shiny::uiOutput(ns("box_plot_desc"), container = tags$p)
    )
  )
  
  # Card 1: Distribution Parameters (GM, GSD) --------------------------------
  
  one <- bslib::card(
    height      = card_height,
    full_screen = TRUE,
    
    bslib::card_header(
      bslib::card_title(
        container = tags$h2,
        class     = "my-2 fs-5",
        shiny::textOutput(ns("one_title"), tags$span)
      )
    ),
    
    bslib::card_header(
      shiny::numericInput(
        inputId = ns("expected_ratio_dist_params"),
        label   = "",
        value   = 0.5,
        min     = 0.001,
        max     = 1000
      )
    ),
    
    bslib::card_body(
      class = "px-5",
      shiny::uiOutput(ns("one"))
    ),
    
    bslib::card_footer(
      shiny::textOutput(ns("one_desc"), tags$p)
    )
  )
  
  # Card 2: Risk Metrics (AM, Percentile) ------------------------------------
  
  two <- bslib::card(
    height      = card_height,
    full_screen = TRUE,
    
    bslib::card_header(
      bslib::card_title(
        container = tags$h2,
        class     = "my-2 fs-5",
        shiny::textOutput(ns("two_title"), tags$span)
      )
    ),
    
    bslib::card_header(
      shiny::numericInput(
        inputId = ns("expected_ratio_metrics"),
        label   = "",
        value   = 0.5,
        min     = 0.001,
        max     = 1000
      )
    ),
    
    bslib::card_body(
      class = "px-5",
      shiny::uiOutput(ns("two"))
    ),
    
    bslib::card_footer(
      shiny::textOutput(ns("two_desc"), tags$p)
    )
  )
  
  # Card 3: Exceedance Fraction ----------------------------------------------
  
  three <- bslib::card(
    height      = card_height,
    full_screen = TRUE,
    
    bslib::card_header(
      bslib::card_title(
        container = tags$h2,
        class     = "my-2 fs-5",
        shiny::textOutput(ns("three_title"), tags$span)
      )
    ),
    
    bslib::card_header(
      shiny::numericInput(
        inputId = ns("expected_delta_exceedance"),
        label   = "",
        value   = 10,
        min     = 0.001,
        max     = 100
      )
    ),
    
    bslib::card_body(
      class = "px-5",
      shiny::uiOutput(ns("three"))
    ),
    
    bslib::card_footer(
      shiny::textOutput(ns("three_desc"), tags$p)
    )
  )
  
  # Card 4: Probability of an Unacceptable Situation -------------------------
  
  four <- bslib::card(
    height      = card_height,
    full_screen = TRUE,
    
    bslib::card_header(
      bslib::card_title(
        container = tags$h2,
        class     = "my-2 fs-5",
        shiny::textOutput(ns("four_title"), tags$span)
      )
    ),
    
    bslib::card_body(
      class = "px-5",
      shiny::uiOutput(ns("four"))
    ),
    
    bslib::card_footer(
      shiny::textOutput(ns("four_desc"), tags$p)
    )
  )
  
  # Panel --------------------------------------------------------------------
  
  ui <- bslib::nav_panel(
    value = id,
    title = shiny::textOutput(ns("title"), tags$span),
    
    bslib::layout_column_wrap(
      width         = 1/2,
      fill          = FALSE,
      heights_equal = "row",
      
      box_plot,
      one,
      two,
      three,
      four
    )
  )
  
  return(ui)
}

#' @rdname ui-panel-categories-comparator-two
#' @export
server_panel_categories_comparator_two <- function(
    id,
    lang,
    inputs_calc,
    data_sample,
    simulations)
{
  stopifnot(exprs = {
    shiny::is.reactive(lang)
    shiny::is.reactive(inputs_calc)
    shiny::is.reactive(data_sample)
    shiny::is.reactive(simulations)
  })
  
  server <- function(input, output, session) {
    
    # Title ----------------------------------------------------------------
    
    title <- shiny::reactive({
      translate(lang = lang(), "Compare Two Categories")
    }) |>
      shiny::bindCache(lang())
    
    output$title <- shiny::renderText({
      title()
    })
    
    # Card titles ----------------------------------------------------------
    
    output$box_plot_title <- shiny::renderText({
      translate(lang = lang(), "Box and Whisker Plot")
    }) |>
      shiny::bindCache(lang())
    
    output$one_title <- shiny::renderText({
      translate(lang = lang(), "Expected Changes in Distribution Parameters")
    }) |>
      shiny::bindCache(lang())
    
    output$two_title <- shiny::renderText({
      translate(lang = lang(), "Expected Changes in Risk Metrics")
    }) |>
      shiny::bindCache(lang())
    
    output$three_title <- shiny::renderText({
      translate(lang = lang(), "Expected Changes in Exceedance Fraction")
    }) |>
      shiny::bindCache(lang())
    
    output$four_title <- shiny::renderText({
      translate(lang = lang(), "Probability of an Unacceptable Situation")
    }) |>
      shiny::bindCache(lang())
    
    # Input labels ---------------------------------------------------------
    
    expected_ratio_dist_params_label <- shiny::reactive({
      translate(lang = lang(), "Expected ratio for GM and GSD (Category 2 / Category 1):")
    })
    
    expected_ratio_metrics_label <- shiny::reactive({
      translate(lang = lang(), "Expected ratio for AM and percentile (Category 2 / Category 1):")
    })
    
    expected_delta_exceedance_label <- shiny::reactive({
      translate(lang = lang(), "Expected difference in exceedance fraction (Category 2 - Category 1, in percentage points):")
    })
    
    # Box plot -------------------------------------------------------------
    
    output$box_plot <- renderPlot({
      lang <- lang()
      inputs_calc <- inputs_calc()
      
      boxplot.2.cat(
        data.formatted    = data_sample(),
        bayesian.output.D = simulations(),
        cat1              = inputs_calc$data_chosen_category,
        cat2              = inputs_calc$data_chosen_category_2,
        boxplot.2cat.1    = inputs_calc$data_chosen_category,
        boxplot.2cat.2    = inputs_calc$data_chosen_category_2,
        boxplot.2cat.3    = translate(lang = lang, "Concentration"),
        boxplot.2cat.4    = translate(lang = lang, "OEL"),
        boxplot.2cat.5    = translate(lang = lang, "Category")
      )
    })
    
    output$box_plot_desc <- shiny::renderUI({
      lang <- lang()
      
      html(
        translate(lang = lang, "
                    Each category has its own box and whisker plot. The
                    measurements are scattered around the midpoint of the
                    x-axis. The box (outer horizontal lines) represents the
                    distance between the %s and %s percentiles. The whisker
                    (vertical line) represents the distance between the %s
                    and %s percentiles. The inner black horizontal line is
                    the median. The OEL is shown as a red line. Shaded and
                    colored points represent the idealized distribution,
                    while bold points represent the actual observations.
                    See Frequently Asked Questions for more information on
                    how non-detects (censored values) are imputed.
                "),
        ordinal(25L, lang),
        ordinal(75L, lang),
        ordinal(10L, lang),
        ordinal(90L, lang)
      )
    }) |>
      shiny::bindCache(lang())
    
    # Card 1: Distribution Parameters (GM, GSD) ----------------------------
    
    output$one <- shiny::renderUI({
      lang <- lang()
      inputs_calc <- inputs_calc()
      
      user.input <- c(
        inputs_calc,
        list(exp.ratio.gm = input$expected_ratio_dist_params)
      )
      
      tbl <- fun.2cat.dist(
        bayesian.output.D = simulations(),
        cat1              = inputs_calc$data_chosen_category,
        cat2              = inputs_calc$data_chosen_category_2,
        user.input        = user.input,
        comp.2cat.1       = translate(lang = lang, "Geometric Mean"),
        comp.2cat.2       = translate(lang = lang, "Geometric Standard Deviation"),
        comp.2cat.3       = translate(lang = lang, "Parameter"),
        comp.2cat.4       = translate(lang = lang, "Category")
      )
      
      as_html_table(tbl)
    })
    
    output$one_desc <- shiny::renderText({
      translate(lang = lang(), "
                Compares the geometric mean and geometric standard deviation
                of the two selected categories. The ratio is Category 2 over
                Category 1. The two probability columns show how often the
                ratio is greater or smaller than the expected ratio above.
            ")
    }) |>
      shiny::bindCache(lang())
    
    
    output$two <- shiny::renderUI({
      lang <- lang()
      inputs_calc <- inputs_calc()
      
      user.input <- c(
        inputs_calc,
        list(exp.ratio.perc = input$expected_ratio_metrics)
      )
      
      tbl <- fun.2cat.metrics(
        bayesian.output.D = simulations(),
        cat1              = inputs_calc$data_chosen_category,
        cat2              = inputs_calc$data_chosen_category_2,
        user.input        = user.input,
        comp.2cat.1       = translate(lang = lang, "Percentile"),
        comp.2cat.2       = translate(lang = lang, "Arithmetic Mean"),
        comp.2cat.3       = translate(lang = lang, "Parameter"),
        comp.2cat.4       = translate(lang = lang, "Category")
      )
      
      as_html_table(tbl)
    })
    
    output$two_desc <- shiny::renderText({
      translate(lang = lang(), "
                Compares the chosen critical percentile and the arithmetic
                mean of the two selected categories. The ratio is Category 2
                over Category 1. The two probability columns show how often
                the ratio is greater or smaller than the expected ratio above.
            ")
    }) |>
      shiny::bindCache(lang())
    
    output$three <- shiny::renderUI({
      lang <- lang()
      inputs_calc <- inputs_calc()
      
      user.input <- c(
        inputs_calc,
        list(expdelta = input$expected_delta_exceedance)
      )
      
      tbl <- fun.2cat.f(
        data.formatted    = data_sample(),
        bayesian.output.D = simulations(),
        cat1              = inputs_calc$data_chosen_category,
        cat2              = inputs_calc$data_chosen_category_2,
        user.input        = user.input,
        comp.2cat.1       = translate(lang = lang, "Exceedance"),
        comp.2cat.3       = translate(lang = lang, "Parameter"),
        comp.2cat.4       = translate(lang = lang, "Category")
      )
      
      as_html_table(tbl)
    })
    
    output$three_desc <- shiny::renderText({
      translate(lang = lang(), "
                Compares the exceedance fraction of the two selected
                categories. The delta column is Category 2 minus Category 1
                in percentage points. The two probability columns show how
                often the delta exceeds (or is smaller than) the expected
                difference above.
            ")
    }) |>
      shiny::bindCache(lang())
    
    output$four <- shiny::renderUI({
      lang <- lang()
      inputs_calc <- inputs_calc()
      
      tbl <- fun.2cat.risk(
        data.formatted    = data_sample(),
        bayesian.output.D = simulations(),
        cat1              = inputs_calc$data_chosen_category,
        cat2              = inputs_calc$data_chosen_category_2,
        user.input        = inputs_calc,
        comp.2cat.1       = translate(lang = lang, "Exceedance Fraction"),
        comp.2cat.2       = translate(lang = lang, "Percentile"),
        comp.2cat.3       = translate(lang = lang, "Arithmetic Mean"),
        comp.2cat.4       = translate(lang = lang, "Parameter"),
        comp.2cat.5       = translate(lang = lang, "Overexposure")
      )
      
      as_html_table(tbl)
    })
    
    output$four_desc <- shiny::renderText({
      translate(lang = lang(), "
                Shows the probability that each category is in an unacceptable
                situation, using three risk criteria: exceedance fraction
                above its threshold, critical percentile above the OEL, and
                arithmetic mean above the OEL. The delta column is Category 2
                minus Category 1, in percentage points.
            ")
    }) |>
      shiny::bindCache(lang())
    
    # Update labels of inputs that aren't rendered via render*() -----------
    
    shiny::observe({
      shiny::updateNumericInput(
        inputId = "expected_ratio_dist_params",
        label   = expected_ratio_dist_params_label()
      )
      shiny::updateNumericInput(
        inputId = "expected_ratio_metrics",
        label   = expected_ratio_metrics_label()
      )
      shiny::updateNumericInput(
        inputId = "expected_delta_exceedance",
        label   = expected_delta_exceedance_label()
      )
    })
    
    return(title)
  }
  
  return(shiny::moduleServer(id, server))
}
