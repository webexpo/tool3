# Tool 3 version `4.0.0`

This major version is a complete redesign of Tool 3. It was entirely rewritten
except for core scientific back-end components. They were left nearly as is in
the source code. The current code base is a refactoring of Tool 1 version
`5.3.0` (RC1) assets as they were on July 21<sup>st</sup>, 2025.

## User Visible Changes

* Tool 3 is now available in English and French. The Language dropdown in the
  Title Bar allows users to switch between them at any time.

* A new **Comparative Analysis** section is available in the top navigation
  bar. It regroups two panels.

  * **Compare All Categories** ranks each category of the chosen Variable of
    Interest against the others, highlighting differences in geometric mean,
    exceedance fraction, critical percentile, and arithmetic mean.

  * **Compare Two Categories** produces a detailed pairwise comparison of two
    specific categories, including ratios of the geometric mean and geometric
    standard deviation, ratios of the arithmetic mean and critical percentile,
    the difference in exceedance fraction, and probabilities that user-defined
    expected ratios or differences are met.

* A new warning banner is shown in the sidebar when more than 80% of the
  submitted measurements (either overall or within the selected category) are
  left-censored. Under such conditions, the posterior increasingly reflects
  prior assumptions rather than the data. The banner recommends using
  IHSTAT_Bayes to assess how sensitive the conclusions are to different prior
  choices.

* A downloadable example file is now linked directly from the Frequently Asked
  Questions panel (*How should my data be formatted?*), letting users try
  Tool 3 with a properly formatted spreadsheet without leaving the interface.

* The Title Bar was revamped following the same patterns as Tool 1: modes
  dropdown, language dropdown, color mode toggle, GitHub button, and FAQ
  button.

* The user interface can now be customized via query parameters (included in
  the URL). Bookmarking a URL preserves the language, mode, and color choices.

* Panels now have a title bar showing the current panel name, and inputs
  specific to a given panel are only shown when that panel is active.

* The Frequently Asked Questions modal was rewritten and reorganized into
  four sections: General, Calculation Parameters, Usage, and Methodology.
  It documents the new Comparative Analysis workflow, the file-format
  requirements, and how censored measurements are handled.

* A new section in the FAQ lists the external collaborators who worked on
  translations.

## Server Changes

* Complete refactoring using the same architecture as Tool 1: `.Rprofile`,
  `app.R`, and per-component modules in `R/`. Legacy Expostats scripts are
  kept unchanged in `scripts/`.

* Internationalization is handled by the
  [transltr](https://cran.r-project.org/package=transltr) package. Source
  strings are automatically extracted from the code base by `.find()`
  (`.scripts/find-text.R`), and translations live in `i18n/`.

* Bayesian calculations are performed via the vendored WebExpo library
  (`scripts/webexpo/`) through an adapter that preserves the historical
  `fun.bayes.jags()` signature.

* Development scripts are exposed as functions: `.run()`, `.find()`, and
  `.pub()`. Publication produces static HTML files from `NEWS.md` and
  `i18n/README.md` and serves them under `/assets`.

* All calls to `translate()` are wrapped in cached reactive values
  (`shiny::bindCache()`) to keep the interface responsive when the language
  is toggled.

* CSS uses `bslib` and Bootstrap 5 utility classes. Custom rules are
  concentrated in `www/main.css` and prefixed by `app-`.

## Fixes

* Fixed a bug in the Descriptive Graphs where the Box and Whiskers Plot
  jittered points vertically because the default `height` argument of
  `ggplot2::position_jitter()` is derived from the data resolution. Vertical
  jitter is now explicitly set to `0`.