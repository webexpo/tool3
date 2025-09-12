#' Categories Comparisons Panel Module
#'
#' This module controls the Categories Comparisons panel component nested into
#' the application's main [bslib::navset].
#'
#' @template param-id
#'
#' @template param-lang
#'
#' @template param-inputs-calc
#'
#' @template param-data-sample
#'
#' @template param-simulations
#'
#' @returns
#' [ui_panel_categories_comparisons()] returns a `shiny.tag` object
#' (an output of [bslib::nav_panel()]).
#'
#' [server_panel_categories_comparisons()] returns a [shiny::reactive()]
#' object. It can be called to get the panel's title.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-panel-categories-comparisons
#' @export
ui_panel_categories_comparisons <- function(id) {
    ns <- shiny::NS(id)
    card_height      <- getOption("app_card_height_md")
    card_height_text <- getOption("app_card_height_sm")

    # Descriptive Statistics ---------------------------------------------------

    stats <- bslib::card(
        height      = card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("stats_title"), tags$span)
            )
        ),

        bslib::card_body(
            class = "px-5",
            shiny::uiOutput(ns("stats"))
        ),

        bslib::card_footer(
            shiny::textOutput(ns("stats_desc"), tags$p)
        )
    )

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

    # Risk Gauge Plot ----------------------------------------------------------

    risk_gauge_plot <- bslib::card(
        height      = card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("risk_gauge_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("risk_gauge_plot"))
        ),

        bslib::card_footer(
            shiny::uiOutput(ns("risk_gauge_plot_desc"), container = tags$p)
        )
    )

    # Risk Band Plot -----------------------------------------------------------

    risk_band_plot <- bslib::card(
        height      = card_height,
        full_screen = TRUE,

        bslib::card_header(
            bslib::card_title(
                container = tags$h2,
                class     = "my-2 fs-5",
                shiny::textOutput(ns("risk_band_plot_title"), tags$span)
            )
        ),

        bslib::card_body(
            shiny::plotOutput(ns("risk_band_plot"))
        ),

        bslib::card_footer(
            shiny::uiOutput(ns("risk_band_plot_desc"), container = tags$p)
        )
    )

    # Panel --------------------------------------------------------------------

    ui <- bslib::nav_panel(
        value = id,
        title = shiny::textOutput(ns("title"), tags$span),

        # This yields the following layout:
        #
        # | -------------------------------- |
        # |              stats               |
        # |    box_plot    | risk_gauge_plot |
        # | risk_band_plot |                 |
        # | -------------------------------- |
        bslib::layout_column_wrap(
            width         = 1L,
            fill          = FALSE,
            heights_equal = "row",

            stats,

            bslib::layout_column_wrap(
                width         = 1/2,
                fill          = FALSE,
                heights_equal = "row",

                box_plot,
                risk_gauge_plot,
                risk_band_plot
            )
        )
    )

    return(ui)
}

#' @rdname ui-panel-categories-comparisons
#' @export
server_panel_categories_comparisons <- function(
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
            translate(lang = lang(), "Categories Comparisons")
        }) |>
        shiny::bindCache(lang())

        data_sample_imputed <- shiny::reactive({
            data_sample <- data_sample()

            simple.censored.treatment(
                observations.formatted = data_sample$data,
                notcensored            = data_sample$notcensored,
                leftcensored           = data_sample$leftcensored,
                rightcensored          = data_sample$rightcensored,
                intcensored            = data_sample$intcensored
            )
        })

        # The order of row names must match
        # what fun.comp.table.D() returns.
        stats_dim_names <- shiny::reactive({
            lang <- lang()

            list(
                rows = c(
                    translate(lang = lang, "Geometric Mean"),
                    translate(lang = lang, "Geometric Standard Deviation"),
                    translate(lang = lang, "Relative Index of Exposure (RIE)"),
                    translate(lang = lang, "Exceedance Fraction"),
                    translate(lang = lang, "Exceedance Fraction's Upper Credible Limit"),
                    translate(lang = lang, "Critical Percentile"),
                    translate(lang = lang, "Critical Percentile's Upper Credible Limit"),
                    translate(lang = lang, "Arithmetic Mean"),
                    translate(lang = lang, "Arithmetic Mean's Upper Credible Limit")
                ),
                first_col = translate(lang = lang, "Parameter")
            )
        }) |>
        shiny::bindCache(lang())

        output$title <- shiny::renderText({
            title()
        })

        output$stats_title <- shiny::renderText({
            translate(lang = lang(), "Descriptive Statistics")
        }) |>
        shiny::bindCache(lang())

        output$box_plot_title <- shiny::renderText({
            translate(lang = lang(), "Box and Whisker Plot")
        }) |>
        shiny::bindCache(lang())

        output$risk_gauge_plot_title <- shiny::renderText({
            translate(lang = lang(), "Risk Gauge Plot")
        }) |>
        shiny::bindCache(lang())

        output$risk_band_plot_title <- shiny::renderText({
            translate(lang = lang(), "Risk Band Plot")
        }) |>
        shiny::bindCache(lang())

        output$stats <- shiny::renderUI({
            stats_dim_names <- stats_dim_names()

            # fun.comp.table.D() misses arguments to consistently
            # rename all rows appropriately. They are overwritten
            # below instead. Argument comp.d.7 is used to rename
            # the first column. Others are named after
            # inputs_calc()$data_categories.
            stats <- fun.comp.table.D(
                bayesian.analysis.D = simulations(),
                c.oel               = data_sample()$c.oel,
                user.input          = inputs_calc(),
                comp.d.7            = stats_dim_names$first_col
            )

            # Overwrite internal row names
            # (stored in the first column).
            stats[[1]] <- stats_dim_names$rows

            as_html_table(stats)
        })

        output$stats_desc <- shiny::renderText({
            translate(lang = lang(), "
                Upper credible limits are determined by the chosen credible
                interval probability. It is assumed that the category having
                the highest geometric mean has a RIE equal to 1 (100%). Other
                categories are relative to it. Therefore, a category having a
                RIE equal to 50% implies that its underlying geometric mean is
                half that of the reference category.
            ")
        })

        output$box_plot <- shiny::renderPlot({
            lang <- lang()

            boxplot.by.cat(
                data.formatted      = data_sample(),
                data.simply.imputed = data_sample_imputed(),
                bayesian.output.D   = simulations(),
                boxplot.cat.1       = translate(lang = lang, "Category"),
                boxplot.cat.2       = translate(lang = lang, "Concentration"),
                boxplot.cat.3       = translate(lang = lang, "OEL")
            )
        })

        output$box_plot_desc <- shiny::renderUI({
            lang <- lang()

            html(
                translate(lang = lang, "
                    Each category having at least three detected (non-censored)
                    results has its own box and whisker plot. The measurements
                    are scattered around the midpoint of the x-axis. The boxes
                    (outer vertical lines) represent the distance between the
                    %s and %s percentiles. The whiskers (horizontal lines)
                    represent the distance between the %s and %s percentiles.
                    Inner vertical lines are the respective medians. The OEL
                    is shown as a red line. See Frequently Asked Questions for
                    more information.
                "),
                ordinal(25L, lang),
                ordinal(75L, lang),
                ordinal(10L, lang),
                ordinal(90L, lang)
            )
        }) |>
        shiny::bindCache(lang())

        output$risk_gauge_plot <- shiny::renderPlot({
            lang <- lang()

            risk.gauge(
                bayesian.ouput.D = simulations(),
                c.oel            = data_sample()$c.oel,
                user.input       = inputs_calc(),
                ggplot.cat.1     = translate(lang = lang, "Category"),
                ggplot.cat.2     = translate(lang = lang, "Overexposure Risk")
            )
        })

        output$risk_gauge_plot_desc <- shiny::renderUI({
            lang <- lang()
            risk_level_1 <- get_risk_level_info(1L, lang)
            risk_level_2 <- get_risk_level_info(2L, lang)
            risk_level_3 <- get_risk_level_info(3L, lang)
            low_threshold <- as_percentage(risk_level_2$threshold)
            high_threshold <- as_percentage(risk_level_3$threshold)

            html(
                translate(lang = lang(), "
                    This plot shows the overexposure risk (the probability that
                    the critical percentile is above the OEL) for categories
                    having at least three detected (non-censored) results,
                    sorted from highest to lowest. Colors indicate whether the
                    risk is deemed %s (lower than %s), %s (between %s and %s),
                    or %s (higher than %s). The decision scheme to interpret
                    the probability of overexposure (the overexposure risk,
                    which is the probability that the overexposure criterion is
                    met) follows the recommendation of the AIHA video series %s
                    (English only). [REVIEW]
                "),
                risk_level_1$name,
                low_threshold,
                risk_level_2$name,
                low_threshold,
                high_threshold,
                risk_level_3$name,
                high_threshold,
                ui_link(
                    "https://www.aiha.org/education/elearning/online-courses/making-accurate-exposure-risk-decisions",
                    "Making Accurate Exposure Risk Decisions"
                )
            )
        }) |>
        shiny::bindCache(lang())

        output$risk_band_plot <- shiny::renderPlot({
            lang <- lang()

            riskband.perc.byband(
                bayesian.ouput.D  = simulations(),
                c.oel             = data_sample()$c.oel,
                target_perc       = inputs_calc()$target_perc,
                sorting           = TRUE,
                # ≤ may not render in all IDEs. This is Unicode
                # character U+2264 (&leq;) (Less-Than or Equal To).
                riskplot.3        = translate(lang = lang, "≤ 1% OEL"),
                riskplot.4        = translate(lang = lang, "1% < OEL ≤ 10%"),
                riskplot.5        = translate(lang = lang, "10% < OEL ≤ 50%"),
                riskplot.6        = translate(lang = lang, "50% < OEL ≤ 100%"),
                riskplot.7        = translate(lang = lang, "> OEL"),
                riskband.byband.1 = translate(lang = lang, "Category"),
                riskband.byband.2 = translate(lang = lang, "Probability"),
                riskband.byband.3 = translate(lang = lang, "Critical Percentile Category")
            )
        })

        output$risk_band_plot_desc <- shiny::renderUI({
            translate(lang = lang(), "
                This plot shows the probability distribution of the
                uncertainty around the selected percentile for categories
                having at least three detected (non-censored) results. It
                shows the probability that its true value is
                (1) below 1% of the OEL,
                (2) between 1% and 10% of the OEL,
                (3) between 10% and 50% of the OEL,
                (4) between 50% and 100% of the OEL, and
                (5) greater than the OEL.
                This is based on the classification adopted by AIHA. Red
                regions represent the probability of an overexposure. [REVIEW]
            ")
        }) |>
        shiny::bindCache(lang())

        return(title)
    }

    return(shiny::moduleServer(id, server))
}
