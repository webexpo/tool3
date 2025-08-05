#' Sidebar Module
#'
#' @description
#' This module controls the Sidebar component conceptually illustrated below.
#'
#' ```
#' ---------------------------------------------
#' | Title                                     |
#' ---------------------------------------------
#' | Sidebar       | Main                      |
#' | (this module) |  -----------------------  |
#' |               |  | Panels Navigation   |  |
#' |               |  -----------------------  |
#' |               |  | Active Panel        |  |
#' |               |  |                     |  |
#' |               |  |                     |  |
#' |               |  |                     |  |
#' |               |  -----------------------  |
#' ---------------------------------------------
#' ```
#'
#' @template param-id
#'
#' @template param-lang
#'
#' @returns
#' [ui_sidebar()] returns a `bslib_sidebar` object
#' (an output of [bslib::sidebar()]).
#'
#' [server_sidebar()] returns a [shiny::reactive()] object. For more
#' information, consult the source text passed to related tooltips in
#' [server_sidebar()] below.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname ui-sidebar
#' @export
ui_sidebar <- function(id) {
    ns <- shiny::NS(id)
    ui <- bslib::sidebar(
        width = "400px",
        gap   = "0.75rem",
        open  = list(
            mobile  = "closed",
            desktop = "open"
        ),

        # Title ----------------------------------------------------------------

        # Setting class sidebar-title is recommended by
        # bslib. Otherwise, the title is ugly and must
        # be fixed with further classes.
        title = tags$h3(
            class = "sidebar-title text-center",

            tags$span(
                class = "pe-2",
                bsicons::bs_icon("calculator-fill", a11y = "deco")
            ),

            shiny::textOutput(ns("title"), tags$span)
        ),

        # Inputs ---------------------------------------------------------------

        shiny::numericInput(
            inputId = ns("oel"),
            label   = "",
            value   = 100
        ) |>
        bslib::tooltip(id = ns("oel_tooltip"), ""),

        shiny::numericInput(
            inputId = ns("oel_multiplier"),
            label   = "",
            value   = 1
        ) |>
        bslib::tooltip(id = ns("oel_multiplier_tooltip"), ""),

        shiny::numericInput(
            inputId = ns("conf"),
            label   = "",
            value   = 90,
            min     = 0,
            max     = 100
        ) |>
        bslib::tooltip(id = ns("conf_tooltip"), ""),

        shiny::numericInput(
            inputId = ns("psi"),
            label   = "",
            value   = 30,
            min     = 0,
            max     = 100
        ) |>
        bslib::tooltip(id = ns("psi_tooltip"), ""),

        shiny::numericInput(
            inputId = ns("frac_threshold"),
            label   = "",
            value   = 5,
            min     = 0,
            max     = 100
        ) |>
        bslib::tooltip(id = ns("frac_threshold_tooltip"), ""),

        shiny::numericInput(
            inputId = ns("target_perc"),
            label   = "",
            value   = 95,
            min     = 0,
            max     = 100
        ) |>
        bslib::tooltip(id = ns("target_perc_tooltip"), ""),

        # FIXME: This used to be identified by 'file1' in version 3. Changing
        # this name may break things.
        file_input(
            inputId     = ns("data"),
            label       = "",
            buttonLabel = "",
            placeholder = "",
            multiple    = FALSE,
            accept      = c(
                "text/csv", # .csv
                "application/vnd.ms-excel", # .xls
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" # .xlsx
            )
        ) |>
        bslib::tooltip(id = ns("data_tooltip"), ""),

        # Buttons --------------------------------------------------------------

        tags$div(
            class = "d-flex justify-content-around",
            style = "gap: 0.5rem; margin-bottom: 1rem;",

            shiny::actionButton(
                inputId = ns("btn_submit"),
                class   = "btn btn-success app-btn w-100",
                label   = shiny::textOutput(ns("btn_submit_label"), tags$span)
            ) |>
            bslib::tooltip(
                id        = ns("btn_submit_tooltip"),
                placement = "bottom",
                ""
            ),

            shiny::actionButton(
                inputId = ns("btn_clear"),
                class   = "btn btn-secondary app-btn w-100",
                label   = shiny::textOutput(ns("btn_clear_label"), tags$span)
            ) |>
            bslib::tooltip(
                id        = ns("btn_clear_tooltip"),
                placement = "bottom",
                ""
            )
        ),

        # Footer ---------------------------------------------------------------

        tags$hr(class = "m-0 mb-2"),

        ui_footer(ns("footer"))
    )

    return(ui)
}

#' @rdname ui-sidebar
#' @export
server_sidebar <- function(id, lang, mode, panel_active) {
    stopifnot(exprs = {
        shiny::is.reactive(lang)
        shiny::is.reactive(mode)
        shiny::is.reactive(panel_active)
    })

    server <- \(input, output, session) {
        server_footer("footer", lang)

        oel_label <- shiny::reactive({
            translate(lang = lang(), "Occupational Exposure Limit (OEL):")
        }) |>
        shiny::bindCache(lang())

        oel_multiplier_label <- shiny::reactive({
            translate(lang = lang(), "OEL Multiplier:")
        }) |>
        shiny::bindCache(lang())

        conf_label <- shiny::reactive({
            translate(lang = lang(), "Credible Interval Probability:")
        }) |>
        shiny::bindCache(lang())

        psi_label <- shiny::reactive({
            translate(lang = lang(), "Overexposure Risk Threshold:")
        }) |>
        shiny::bindCache(lang())

        frac_threshold_label <- shiny::reactive({
            translate(lang = lang(), "Exceedance Fraction Threshold:")
        }) |>
        shiny::bindCache(lang())

        target_perc_label <- shiny::reactive({
            translate(lang = lang(), "Critical Percentile:")
        }) |>
        shiny::bindCache(lang())

        data_label <- shiny::reactive({
            translate(lang = lang(), "Measurements:")
        }) |>
        shiny::bindCache(lang())

        data_btn_label <- shiny::reactive({
            translate(lang = lang(), "Browse")
        }) |>
        shiny::bindCache(lang())

        oel_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to assess overexposure. It must have the same
                unit as the measurement data.
            ")
        }) |>
        shiny::bindCache(lang())

        oel_multiplier_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to modify the OEL. The value used in subsequent
                calculations is the product of the OEL (see above) and this
                factor. It can be used to determine a protection factor for
                respiratory protection.
            ")
        }) |>
        shiny::bindCache(lang())

        conf_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value as a probability for the credible intervals
                around parameter estimates. It must be between 0% and 100%.
                The default value is set equal to 90%. The credible interval
                is the Bayesian equivalent of the confidence interval.
            ")
        }) |>
        shiny::bindCache(lang())

        psi_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value as the maximal overexposure risk. It must be
                between 0% and 100%. It represents the maximal probability
                that the overexposure criterion is met. Above this value,
                the situation requires remedial action. While 5% is the
                traditional chosen value, recent guidelines suggest using
                30% instead.
            ")
        }) |>
        shiny::bindCache(lang())

        frac_threshold_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value as an acceptable proportion of exposures above
                the OEL. It must be between 0% and 100%. The traditional default
                value is 5%.
            ")
        }) |>
        shiny::bindCache(lang())

        target_perc_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Use this value to set the percentile of the exposure
                distribution that will be compared to the OEL. It must
                be between 0% and 100%. The traditional default value
                is 95%.
            ")
        }) |>
        shiny::bindCache(lang())

        data_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                The measurement dataset. There must be one value per line.
                Values can be censored to the left (<), to the right (>),
                or interval censored ([X-Y]). For more information, see the
                Calculation Parameters section in Frequently Asked Questions
                (FAQ) above.
            ")
        }) |>
        shiny::bindCache(lang())

        btn_submit_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Submit all parameters and start Bayesian calculations.
            ")
        }) |>
        shiny::bindCache(lang())

        btn_clear_tooltip_text <- shiny::reactive({
            translate(lang = lang(), "
                Clear the measurement dataset. Doing so does not
                automatically update the current results. Click
                on the Submit button when you are ready to do so.
            ")
        }) |>
        shiny::bindCache(lang())

        output$title <- shiny::renderText({
            translate(lang = lang(), "Calculation Parameters")
        }) |>
        shiny::bindCache(lang())

        output$btn_submit_label <- shiny::renderText({
            translate(lang = lang(), "Submit")
        }) |>
        shiny::bindCache(lang())

        output$btn_clear_label <- shiny::renderText({
            translate(lang = lang(), "Clear")
        }) |>
        shiny::bindCache(lang())

        # FIXME: Deactivated until further notice.
        # Show inputs that are specific to certain panels.
        # Identifiers are hardcoded (they will never change).
        # shiny::observe({
        #     panel_active <- panel_active()

        #     # Since inputs are hidden by default, operator == is used.
        #     shinyjs::toggle("frac_threshold", condition = {
        #         panel_active == "panel_fraction"
        #     })
        #     shinyjs::toggle("target_perc", condition = {
        #         panel_active == "panel_percentiles"
        #     })
        # }) |>
        # shiny::bindEvent(panel_active())

        # Translate elements not rendered
        # with a shiny::render*() function.
        shiny::observe({
            shiny::updateNumericInput(inputId = "oel", label = oel_label())
            shiny::updateNumericInput(inputId = "oel_multiplier", label = oel_multiplier_label())
            shiny::updateNumericInput(inputId = "conf", label = conf_label())
            shiny::updateNumericInput(inputId = "psi", label = psi_label())
            shiny::updateNumericInput(inputId = "frac_threshold", label = frac_threshold_label())
            shiny::updateNumericInput(inputId = "target_perc", label = target_perc_label())

            update_file_input(
                inputId     = "data",
                label       = data_label(),
                buttonLabel = data_btn_label()
            )

            bslib::update_tooltip("oel_tooltip", oel_tooltip_text())
            bslib::update_tooltip("oel_multiplier_tooltip", oel_multiplier_tooltip_text())
            bslib::update_tooltip("conf_tooltip", conf_tooltip_text())
            bslib::update_tooltip("psi_tooltip", psi_tooltip_text())
            bslib::update_tooltip("frac_threshold_tooltip", frac_threshold_tooltip_text())
            bslib::update_tooltip("target_perc_tooltip", target_perc_tooltip_text())
            bslib::update_tooltip("data_tooltip", data_tooltip_text())
            bslib::update_tooltip("btn_submit_tooltip", btn_submit_tooltip_text())
            bslib::update_tooltip("btn_clear_tooltip", btn_clear_tooltip_text())
        })

        # Return all inputs except buttons.
        # FIXME: Overall, the reading mechanism below is temporary.
        return(
            shiny::reactive({
                # FIXME: We may need to change default value.
                read <- switch(input$data$type,
                    "text/csv" = data.table::fread, # .csv
                    "application/vnd.ms-excel" = readxl::read_xls, # .xls
                    "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = readxl::read_xlsx, # .xlsx,
                    NULL
                )

                # FIXME: We may need to return errors to the user.
                data <- tryCatch(
                    read(input$data$datapath),
                    condition = \(cond) NULL
                )

                list(
                    oel            = input$oel,
                    data           = input$data,
                    oel_multiplier = input$oel_multiplier,
                    conf           = input$conf,
                    psi            = input$psi,
                    frac_threshold = input$frac_threshold,
                    target_perc    = input$target_perc
                )
            }) |>
            shiny::bindEvent(input$btn_submit, ignoreInit = TRUE)
        )
    }

    return(shiny::moduleServer(id, server))
}
