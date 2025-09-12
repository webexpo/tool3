#' Tool 3: Determinants of Exposure
#'
#' @description
#' User interface and server-side logic.
#'
#' @usage
#' ## In interactive sessions
#' .run()
#'
#' @format
#' `ui` is a `bslib_page` object (an output of [bslib::page_sidebar()]). This
#' is a list of `shiny.tag` objects.
#.
#' @returns [server()] returns `NULL`, invisibly.
#'
#' @author Jérôme Lavoué (<jerome.lavoue@@umontreal.ca>)
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname app
#' @export
ui <- bslib::page_sidebar(
    lang         = "en",
    window_title = "Expostats - Tool 3",
    theme        = bslib::bs_theme(5L, "shiny"),
    title        = ui_title("layout_title"),

    # ui_sidebar() returns an attribute named btn_submit_id
    # which is the namespaced identifier of the main Submit
    # button of the Sidebar module. It is required to attach
    # an ExtendedTask to it. See below in server().
    sidebar = sidebar <- ui_sidebar("layout_sidebar"),

    # <head> -------------------------------------------------------------------

    ui_head("head"),

    # <main> -------------------------------------------------------------------

    ui_banner("busy_banner"),

    bslib::navset_card_underline(
        id       = "panel_active",
        selected = "panel_stats",
        header   = bslib::card_body(
            fillable = FALSE,
            fill     = FALSE,
            gap      = 0L,
            padding  = 0L,

            # Panel's title.
            bslib::card_header(
                class = "d-flex flex-column",
                style = "gap: 8px;",

                bslib::card_title(
                    container = tags$h2,
                    class     = "mt-2 fs-5 opacity-75",

                    shiny::textOutput("panel_title", tags$span),

                    # This buttons toggles the container of
                    # the panel's short description below.
                    ui_plain_action_button(
                        inputId = "btn_panel_description",
                        label   = bsicons::bs_icon("caret-up-fill"),
                        class   = "ms-1 p-0",
                    ) |>
                    bslib::tooltip(id = "btn_panel_description_tooltip", "")
                )
            ),

            # Panel's short description.
            bslib::card_header(
                id = "panel_description_header",

                tags$small(
                    class = "opacity-75",
                    shiny::textOutput("panel_description", tags$span)
                )
            )
        ),

        ui_panel_descriptive_statistics("panel_stats"),

        bslib::nav_menu(
            value = "menu_global",
            title = shiny::textOutput("menu_global_title", tags$span),
            icon  = ui_menu_icon(),

            ui_panel_exceedance_fraction("panel_global_fraction"),
            ui_panel_percentiles("panel_global_percentiles"),
            ui_panel_arithmetic_mean("panel_global_mean")
        ),

        bslib::nav_menu(
            value = "menu_single",
            title = shiny::textOutput("menu_single_title", tags$span),
            icon  = ui_menu_icon(),

            ui_panel_exceedance_fraction("panel_single_fraction"),
            ui_panel_percentiles("panel_single_percentiles"),
            ui_panel_arithmetic_mean("panel_single_mean"),

            # Add a divider and a header explaining why
            # there is a second About My Measurements panel.
            bslib::nav_item(tags$hr(class = "dropdown-divider")),

            shiny::textOutput("menu_single_panel_stats_header", tags$span) |>
            htmltools::tagAppendAttributes(class = "dropdown-header") |>
            bslib::nav_item(),

            ui_panel_descriptive_statistics("panel_single_stats")
        ),

        bslib::nav_menu(
            value = "menu_categories",
            title = shiny::textOutput("menu_categories_title", tags$span),
            icon  = ui_menu_icon(),

            ui_panel_categories_comparator_all("panel_categories_comparator_all"),
            "Two Categories"
        )
    )
)

#' @rdname app
#' @export
server <- function(input, output, session) {
    # Step 1: Collect calculation parameters from
    # user and format the data sample it provided.
    data_sample <- shiny::reactive({
        inputs_calc <- inputs_calc()

        data.formatting.D(
            data.in       = inputs_calc$data,
            oel           = inputs_calc$oel,
            oel.mult      = inputs_calc$oel_multiplier,
            VarOfInterest = inputs_calc$data_chosen_variable
        )
    })

    # Step 2: Generate Bayesian simulations of the
    # mean and standard deviation on the log scale
    # for Global Risk Analysis Panels.

    # Step 2.1: Create a shiny::ExtendedTask object to
    # be invoked (executed) later. Its sole purpose is
    # to call Webexpo's simulation functions.
    simulations_task <- shiny::ExtendedTask$new(
        function(
            data_sample          = list(),
            data_chosen_category = "",
            menu_active          = "",
            n_iter               = getOption("app_number_bayes_iter"))
        {
            future::future(conditions = NULL, {
                switch(menu_active,
                    global = fun.bayes.jags(
                        observations  = data_sample$data,
                        notcensored   = data_sample$notcensored,
                        leftcensored  = data_sample$leftcensored,
                        rightcensored = data_sample$rightcensored,
                        intcensored   = data_sample$intcensored,
                        seed          = data_sample$seed,
                        c.oel         = data_sample$c.oel,
                        n.iter        = n_iter
                    ),

                    single = fun.bayes.jags.D(
                        data.formatted = data_sample,
                        n.iter         = n_iter
                    ) |>
                    bayesian.output.B.single(cat = data_chosen_category),

                    # categories, or none (default).
                    fun.bayes.jags.D(
                        data.formatted = data_sample,
                        n.iter         = n_iter
                    )
                )
            })
        }
    ) |>
    bslib::bind_task_button(attr(sidebar, "btn_submit_id", TRUE))

    # Step 2.2: Invoke the shiny::ExtendedTask
    # object. Since it returns immediately,
    # $result() cannot be called right after
    # $invoke(). This is deferred to Shiny's
    # flush cycle.
    invoke_simulations_task <- shiny::reactive({
        simulations_task$invoke(
            data_sample(),
            inputs_calc()$data_chosen_category,
            menu_active()
        )
    }) |>
    shiny::bindEvent(data_sample(), menu_active())

    # Step 2.3: Invoke simulations_task_global(),
    # and fetch results once they are ready.
    simulations <- shiny::reactive({
        invoke_simulations_task()
        simulations_task$result()
    })

    # Step 3: Format results.
    results <- shiny::reactive({
        # data_sample() computes c.oel
        # form oel and oel_multiplier.
        data_sample <- data_sample()
        inputs_calc <- inputs_calc()
        simulations <- simulations()

        psi <- inputs_calc$psi

        results <- all.numeric(
            conf           = inputs_calc$conf,
            frac_threshold = inputs_calc$frac_threshold,
            target_perc    = inputs_calc$target_perc,
            c.oel          = data_sample$c.oel,
            mu.chain       = simulations$mu.chain,
            sigma.chain    = simulations$sigma.chain
        )

        gm   <- results$gm
        gsd  <- results$gsd
        frac <- lapply(results$frac, as_percentage)
        perc <- results$perc
        am   <- results$am

        # Formatted outputs are prefixed by a dot to
        # distinguish them from results stemming from
        # all.numeric().
        formatted <- list(
            .rounded = rapply(results, signif,
                how    = "replace",
                digits = getOption("app_n_signif_digits")
            ),
            .intervals = list(
                gm   = as_interval(gm$est, gm$lcl, gm$ucl),
                gsd  = as_interval(gsd$est, gsd$lcl, gsd$ucl),
                frac = as_interval(frac$est, frac$lcl, frac$ucl),
                perc = as_interval(perc$est, perc$lcl, perc$ucl),
                am   = as_interval(am$est, am$lcl, am$ucl)
            ),
            .risk_levels = list(
                frac    = if (results$frac.risk >= psi) 3L else 1L,
                perc    = if (results$perc.risk >= psi) 3L else 1L,
                am      = if (results$am.risk   >= psi) 3L else 1L,
                express = findInterval(results$perc.risk, get_risk_level_thresholds())
            )
        )

        c(results, formatted)
    })

    # Other Reactives ----------------------------------------------------------

    menu_active <- shiny::reactive({
        switch(input$panel_active,
            panel_global_fraction           = "global",
            panel_global_percentiles        = "global",
            panel_global_mean               = "global",
            panel_single_fraction           = "single",
            panel_single_percentiles        = "single",
            panel_single_mean               = "single",
            panel_single_stats              = "single",
            panel_categories_comparator_all = "categories",
            "none"
        )
    })

    btn_panel_description_tooltip_text <- shiny::reactive({
        translate(lang = lang(), "
            Toggle the panel's short description below.
        ")
    }) |>
    shiny::bindCache(lang())

    # Modules ------------------------------------------------------------------

    # This returns a list of shiny::reactive()
    # objects that can be used to get the UI's
    # current language, mode, and color.
    inputs_ui <- server_title("layout_title")
    lang <- inputs_ui$lang

    # This returns a shiny::reactive() object
    # returning a named list containing all
    # user inputs in a list.
    inputs_calc <- server_sidebar(
        id           = "layout_sidebar",
        lang         = lang,
        panel_active = shiny::reactive({ input$panel_active })
    )

    server_banner(id = "busy_banner", lang = lang)

    # Each server_panel_*() function below returns a
    # shiny::reactive() object that can be called to
    # get the underlying panel's title.
    panel_stats_title <- server_panel_descriptive_statistics(
        id             = "panel_stats",
        lang           = lang,
        inputs_calc    = inputs_calc,
        data_sample    = data_sample,
        use_categories = TRUE
    )

    panel_global_fraction_title <- server_panel_exceedance_fraction(
        id          = "panel_global_fraction",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_global_percentiles_title <- server_panel_percentiles(
        id          = "panel_global_percentiles",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_global_mean_title <- server_panel_arithmetic_mean(
        id          = "panel_global_mean",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_single_fraction_title <- server_panel_exceedance_fraction(
        id          = "panel_single_fraction",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_single_percentiles_title <- server_panel_percentiles(
        id          = "panel_single_percentiles",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_single_mean_title <- server_panel_arithmetic_mean(
        id          = "panel_single_mean",
        lang        = lang,
        inputs_calc = inputs_calc,
        simulations = simulations,
        results     = results
    )

    panel_single_stats_title <- server_panel_descriptive_statistics(
        id             = "panel_single_stats",
        lang           = lang,
        inputs_calc    = inputs_calc,
        data_sample    = data_sample,
        use_categories = FALSE
    )

    panel_categories_comparator_all_title <- server_panel_categories_comparator_all(
        id          = "panel_categories_comparator_all",
        lang        = lang,
        inputs_calc = inputs_calc,
        data_sample = data_sample,
        simulations = simulations
    )

    # Outputs ------------------------------------------------------------------

    output$menu_global_title <- shiny::renderText({
        translate(lang = lang(), "Global Risk Analysis")
    }) |>
    shiny::bindCache(lang())

    output$menu_single_title <- shiny::renderText({
        translate(lang = lang(), "Single-Category Risk Analysis")
    }) |>
    shiny::bindCache(lang())

    output$menu_categories_title <- shiny::renderText({
        translate(lang = lang(), "Categories Comparisons")
    }) |>
    shiny::bindCache(lang())

    output$menu_single_panel_stats_header <- shiny::renderText({
        translate(lang = lang(), "Check Measurements Subsets")
    }) |>
    shiny::bindCache(lang())

    output$panel_title <- shiny::renderText({
        switch(input$panel_active,
            panel_stats                     = panel_stats_title(),
            panel_global_fraction           = panel_global_fraction_title(),
            panel_global_percentiles        = panel_global_percentiles_title(),
            panel_global_mean               = panel_global_mean_title(),
            panel_single_fraction           = panel_single_fraction_title(),
            panel_single_percentiles        = panel_single_percentiles_title(),
            panel_single_mean               = panel_single_mean_title(),
            panel_single_stats              = panel_single_stats_title(),
            panel_categories_comparator_all = panel_categories_comparator_all_title(),
        )
    }) |>
    shiny::bindCache(input$panel_active, lang())

    # Descriptions change based on the context and on
    # how panels are meant to be used. Therefore, they
    # are kept here, outside of the underlying modules
    # (contrarily to titles).
    output$panel_description <- shiny::renderText({
        lang <- lang()
        switch(input$panel_active,
            panel_stats = translate(lang = lang, "
                Use this panel only to ensure that measurements were parsed as
                expected. See Frequently Asked Questions for more information.
            "),
            panel_global_fraction = translate(lang = lang, "
                Use this panel to perform a risk analysis on all measurements,
                without taking into account any stratification variable, and
                using the exceedance fraction as the risk metric.
            "),
            panel_global_percentiles = translate(lang = lang, "
                Use this panel to perform a risk analysis on all measurements,
                without taking into account any stratification variable, and
                using the chosen critical percentile as the risk metric.
            "),
            panel_global_mean = translate(lang = lang, "
                Use this panel to perform a risk analysis on all measurements,
                whole, without taking into account any stratification variable,
                and using the arithmetic mean as the risk metric.
            "),
            panel_single_fraction = translate(lang = lang, "
                Use this panel to perform a risk analysis on a subset of the
                measurements determined by a category of a variable of interest
                (treated as a stratification variable), and using the exceedance
                fraction as the risk metric.
            "),
            panel_single_percentiles = translate(lang = lang, "
                Use this panel to perform a risk analysis on a subset of the
                measurements determined by a category of a variable of interest
                (treated as a stratification variable), and using the chosen
                critical percentile as the risk metric.
            "),
            panel_single_mean = translate(lang = lang, "
                Use this panel to perform a risk analysis on a subset of the
                measurements determined by a category of a variable of interest
                (treated as a stratification variable), and using the arithmetic
                mean as the risk metric.
            "),
            panel_single_stats = translate(lang = lang, "
                Use this panel only to ensure that the subset of measurements
                determined by a category of a variable of interest was parsed
                as expected. See Frequently Asked Questions for more information.
            "),
            panel_categories_comparator_all = translate(lang = lang, "
                Use this panel to compare subsets of measurements determined by
                categories of a variable of interest (treated as a
                stratification variable).
            "),
            # Default case (never used).
            NULL
        )
    }) |>
    shiny::bindCache(input$panel_active, lang())

    # Observers ----------------------------------------------------------------

    # Update the lang attribute of the root <html> tag.
    # See www/main.js for more information.
    shiny::observe(priority = 1L, {
        session$sendCustomMessage("update_page_lang", lang())
    }) |>
    shiny::bindEvent(lang())

    shiny::observe({
        # Descriptions are shown by default. Therefore, the icon
        # to hide them is shown first. Each click toggles between
        # hide (0, 2, 4, ...) and show (1, 3, 5, ...).
        icon_name <- if (input$btn_panel_description %% 2L == 0L) {
            "caret-up-fill"
        } else {
            "caret-down-fill"
        }

        shinyjs::toggle("panel_description_header")

        # Hide the tooltip on each click.
        bslib::toggle_tooltip("btn_panel_description_tooltip")

        shiny::updateActionButton(
           inputId = "btn_panel_description",
           label   = bsicons::bs_icon(icon_name)
        )
    }) |>
    shiny::bindEvent(input$btn_panel_description)

    # Translate elements not rendered
    # with a shiny::render*() function.
    shiny::observe({
        bslib::update_tooltip(
            "btn_panel_description_tooltip",
            btn_panel_description_tooltip_text()
        )
    })
}

#' @rdname app
#' @export
app <- shiny::shinyApp(ui, server)
