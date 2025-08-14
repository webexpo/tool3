#' Package future's Entry Point
#'
#' This sets how shiny::ExtendedTask should be evaluated. For more
#' information on available strategies, see [future::plan()].
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)

switch(Sys.getenv("R_CONFIG_ACTIVE"),
    # TODO: Revisit usefulness of other plans later.
    shinyapps = future::plan("sequential"),
    # Use a single R process otherwise.
    future::plan("sequential")
)
