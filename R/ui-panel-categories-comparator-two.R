#' @rdname ui-panel-categories-comparator-two
#' @export
ui_panel_categories_comparator_two <- function(id) {
    ns <- shiny::NS(id)
    card_height      <- getOption("app_card_height_md")
    card_height_text <- getOption("app_card_height_sm")

    # Box Plot -----------------------------------------------------------------

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

    # Descriptive Statistics ---------------------------------------------------

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
                min     = 0,
                max     = 1
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

    # Panel --------------------------------------------------------------------

    ui <- bslib::nav_panel(
        value = id,
        title = shiny::textOutput(ns("title"), tags$span),

        bslib::layout_column_wrap(
            width         = 1/2,
            fill          = FALSE,
            heights_equal = "row",

            box_plot,
            one
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
        title <- shiny::reactive({
            translate(lang = lang(), "Compare Two Categories")
        }) |>
        shiny::bindCache(lang())

        expected_ratio_dist_params_label <- shiny::reactive({
            translate(lang = lang(), "expected changes (as a ratio) for the GM and GSD")
        })

        output$title <- shiny::renderText({
            title()
        })

        output$box_plot_title <- shiny::renderText({
            translate(lang = lang(), "Box and Whisker Plot")
        }) |>
        shiny::bindCache(lang())

        output$one_title <- shiny::renderText({
            translate(lang = lang(), "
                Expected Changes in Distribution Parameters
            ")
        }) |>
        shiny::bindCache(lang())

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
                    measurements are scattered around the around the midpoint
                    of the x-axis. The box (outer horizontal lines) represents
                    the distance between the %s and %s percentiles. The whisker
                    (vertical line) represent the distance between the %s and
                    %s percentiles. The inner black horizontal line is the
                    median. The OEL is shown as a red line. Shaded and colored
                    points represent the idealized distribution, while bold
                    points represent the actual observations. See Frequently
                    Asked Questions for more information on how non-detects
                    (censored values) are imputed.
                "),
                ordinal(25L, lang),
                ordinal(75L, lang),
                ordinal(10L, lang),
                ordinal(90L, lang)
            )
        }) |>
        shiny::bindCache(lang())

        output$one <- shiny::renderUI({
            lang <- lang()
            inputs_calc <- inputs_calc()
            browser()

            tbl <- fun.2cat.dist(
                bayesian.output.D = simulations(),
                cat1              = inputs_calc$data_chosen_category,
                cat2              = inputs_calc$data_chosen_category_2,
                user.input        = inputs_calc,
                comp.2cat.1       = translate(lang = lang, "Geometric Mean"),
                comp.2cat.2       = translate(lang = lang, "Geometric Standard Deviation"),
                comp.2cat.3       = translate(lang = lang, "Parameter"),
                comp.2cat.4       = translate(lang = lang, "Category")
            )

            # Overwrite internal row names
            # (stored in the first column).
            # stats[[1]] <- stats_dim_names$rows

            as_html_table(tbl)
        })

        output$one_desc <- shiny::renderText({
            translate(lang = lang(), "
                Hello, world!
            ")
        })

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            shiny::updateNumericInput(
                inputId = "expected_ratio_dist_params",
                label   = expected_ratio_dist_params_label()
            )
        })

        return(title)
    }

    return(shiny::moduleServer(id, server))
}
