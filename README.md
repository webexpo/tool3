<div align="center">

<!-- Expostats' logo -->
<img src="www/android-chrome-192x192.png" alt="Expostats's logo" height="192" width="192" />

# Tool 3: Determinants of Exposure

<!-- badges: start -->
[![Version](https://img.shields.io/badge/version-4.0.0-blue)](https://github.com/webexpo/tool3/releases/tag/v4.0.0)
[![Lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Location](https://img.shields.io/badge/live-shinyapps.io-5b90bf)](https://lavoue.shinyapps.io/tool3/)
[![License](https://img.shields.io/badge/license-MIT-orange.svg)](https://github.com/webexpo/tool1/blob/main/LICENSE.md)
<!-- badges: end -->

</div>

A Shiny application developed by the Industrial Hygiene team of the Department
of Environmental and Occupational Health at the
[École de Santé Publique](https://espum.umontreal.ca/english/home/) of the
[Université de Montréal](https://www.umontreal.ca/en/) (EPSUM) in collaboration
with [Ununoctium](https://ununoctium.dev) (Jean-Mathieu Potvin).
[Ununoctium](https://ununoctium.dev) designed the user interface (for versions
greater than or equal to `4.0.0`) and currently maintains the web application
(but not scripts stored in `scripts/`).

Tool 3 uses the same patterns and mechanisms as
[Tool 1](https://github.com/webexpo/tool1) (whenever possible) for
consistency between applications of Expostats.

## Introduction

This tool compares the underlying distributions corresponding to several
categories of a variable of interest (e.g. presence / absence of ventilation,
season of sampling, etc.). Data is entered as a spreadsheet file (Excel file)
containing exposure levels and variables of interest. It exposes three main
components.

1. Group risk assessment based on an exceedance fraction, a percentile, or the
   arithmetic mean.
2. Individual risk assessment based on an exceedance fraction, a percentile, or
   the arithmetic mean.
3. Comparison of two categories highlighting differences between their respective
   geometric means and geometric standard deviations, risk assessments, and
   probabilities that a prespecified difference exists.

Calculations are performed using a Bayesian model fit using a Monte Carlo
Markov Chain (MCMC) engine. It assumes that the underlying exposure distribution
is lognormal.

The underlying Bayesian models and data interpretation procedures are derived
from best practices in industrial hygiene data interpretation techniques. They
are thoroughly described in
*[Expostats: A Bayesian Toolkit to Aid the Interpretation of Occupational Exposure Measurements](https://doi.org/10.1093/annweh/wxy100)*
(Annals of Work Exposures and Health, Volume 63, Issue 3, April 2019, Pages
267–279).

## Requirements

**TO BE COMPLETED.**

R version `4.4.0` is required to work on and serve Tool 3 locally. These
packages (and their transitive dependencies) are required.

- `shiny`
- `bslib`
- `ggplot2`
- `ggimage`
- `htmltools`
- `rjags`
- `randtoolbox`
- `transltr`

## Usage

To serve Tool 3 locally, call

```r
.run()
```

`.run()` is defined in `.scripts/run.R`.

## Deploy

Tool 3 is deployed to [shinyapps.io](https://lavoue.shinyapps.io/tool3).
To deploy a new version, call `.pub()`.

```r
# Deploy a beta version (useful for testing purposes).
# It is publicly accessible at https://lavoue.shinyapps.io/tool3-beta/.
.pub()

# Deploy an official (production) version.
# It is publicly accessible at https://lavoue.shinyapps.io/tool3/.
.pub("prod")
```

`.pub()` is defined in `.scripts/publish.R`.

Some environment variables are required to publish new releases with `.pub()`.
They must be stored in a top-level `.Renviron` file as shown below. This file is
ignored by Git and `rsconnect`.

```
# .Renviron
RSCONNECT_ACCOUNT_NAME=<account>
RSCONNECT_ACCOUNT_TOKEN=<token>
RSCONNECT_ACCOUNT_SECRET=<secret>
```

## General Structure

The project is organized as a standard Shiny application as follows.

```
./
├── .local/
|   └── [temporary files ignored by Git scoped to each developer]
|
├── .scripts/
|   └── [scripts used while developing and ignored at runtime]
|
├── i18n/
|   └── [data used for internationalization (i18n) purposes]
|
├── man-roxygen/
|   └── [shared/common roxygen2 tags used more than once]
|
├── R/
|   └── [objects, constants, functions, etc.]
|
├── rsconnect/
|   └── [metadata on latest deployments]
|
├── scripts/
|   └── [official Expostats scripts and functions]
|
├── wwww/
│   ├── assets/
│   |   └── images/
|   |       └── [static images (any format)]
|   └── [static files served at runtime under root path]
|
└── [usual top-level source files]
```

Most objects are defined in `.Rprofile`, `app.R`, and in `R/`. For historical
reasons, all Expostats functions are stored in `scripts/`. They are explicitly
sourced at runtime by `R/global.R`. Other files are sourced automatically.

For more information on the structure, static assets, and naming conventions,
read subsection
[General Structure](https://github.com/webexpo/tool1?tab=readme-ov-file#general-structure)
of of Tool 1's `README`

## Internationalization (i18n)

> We are actively looking for external collaborators who could help us with
> supporting more languages. If you are interested, please send an e-mail
> to <jerome.lavoue@umontreal.ca>.

Tool 3 uses the same internationalization mechanisms and patterns as Tool 1.
Please read subsection
[Internationalization](https://github.com/webexpo/tool1?tab=readme-ov-file#internationalization-i18n)
of Tool 1's `README` for more information.

## Styling

Tool 3 uses the same styling mechanisms and patterns as Tool 1. Please read
subsection [Styling](https://github.com/webexpo/tool1?tab=readme-ov-file#styling)
of Tool 1's `README` for more information.

It is worthwhile to note that it is **not** necessary to include
[Bootstrap's assets](https://getbootstrap.com/docs/5.3/getting-started/download/#cdn-via-jsdelivr)
(in the `<head>` of Tool 1) because `bslib` already does that automatically.

## Known Issues

**TO BE COMPLETED.**

We may work on these issues in a near future.

- Inputs lack robust validation mechanisms and may lead to undefined behavior
  in some cases.

- Accessibility mechanisms
  ([ARIA](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA)) are
  not implemented.

- Margins, aspect ratios, font sizes, font families, dimensions, and colors of
  all plots are inconsistent and must be standardized.
  - This could be achieved with `shiny::getCurrentOutputInfo()` and
    `bslib::bs_current_theme()`.

- There are currently no explicit Terms of Service and Privacy Policy.

## Bugs and Feedback

You may submit bugs, request features, and provide feedback by creating an
[issue on GitHub](https://github.com/webexpo/app-tool1/issues/new).

If you do not have a GitHub account, send an email to <jerome.lavoue@umontreal.ca>
**and** <jeanmathieupotvin@ununoctium.dev>. Please use the following standard
subject line:

```
Expostats [Tool 3] [Bug|Feedback]: <short description>
```
