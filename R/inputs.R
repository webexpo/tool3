#' File Upload Control
#'
#' Extend [shiny::fileInput()] by allowing it to be changed (on the client) at
#' runtime.
#'
#' [shiny::fileInput()] has no associated `updateFileInput()` function like
#' other Shiny inputs. [file_input()] extends the former in a way that allows
#' [update_file_input()] to work as other usual `shiny::update*()` functions.
#'
#' [file_input()] also changes the style of the output of [shiny::fileInput()].
#'
#' @param ... Further arguments passed to [shiny::fileInput()].
#'
#' @param session A [shiny::session] object.
#'
#' @param inputId A character string. The id of the input object. Omit the
#'   `session`'s underlying namespace.
#'
#' @param label A `shiny.tag` object, an atomic value, or `NULL`. The label to
#'   set for the input object.
#'
#' @param buttonLabel A character string or `NULL`. The label to set for the
#'   input object's button used to browse for a file.
#'
#' @param placeholder A character string or `NULL`. The text to display as a
#'   temporary placeholder (before a file is chosen).
#'
#' @returns
#' [file_input()] returns a `shiny.tag` object.
#'
#' [update_file_input()] returns `NULL`, invisibly. It is used for its side-
#' effect of updating the underlying input on the client's side.
#'
#' @note
#' [update_file_input()] requires a client-side JavaScript function to work
#' properly. See Shiny custom message handler `update_file_input` in
#' www/main.js.
#'
#' @seealso [fileInput()]
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname file-input
#' @export
file_input <- function(...) {
    input <- shiny::fileInput(...)
    query <- htmltools::tagQuery(input)

    # Unique identifiers derived from inputId are
    # added to components that could be updated
    # later on with update_file_input().
    id <- list(...)$inputId

    # "Flexify" the inner container for Browse
    # button and chosen file input and add a
    # small gap betwen elements.
    query$find("div.input-group")$
        addClass("d-flex")$
        addAttrs(style = "gap: 0.5rem;")

    # Enforce usual styles on the Browse
    # button and normalize its border radius.
    query$find("span.btn")$
        addClass("btn-secondary app-btn rounded")$
        addAttrs(id = sprintf("%s-btn-browse", id))

    # Normalize border radius of the chosen file readonly input.
    query$find("input.form-control")$
        addClass("rounded")$
        addAttrs(id = sprintf("%s-input-filename", id))

    # Hide ugly progress bar.
    # It already has an id: "<inputId>_progress".
    query$find("div.progress")$addClass("d-none")

    return(query$allTags())
}

#' @rdname file-input
#' @export
update_file_input <- function(
    session = shiny::getDefaultReactiveDomain(),
    inputId,
    label       = NULL,
    buttonLabel = NULL,
    placeholder = NULL)
{
    stopifnot(exprs = {
        inherits(session, c(
            "ShinySession",
            "MockShinySession",
            "session_proxy")
        )
        is_chr1(inputId)
        is.null(label) || is.atomic(label) || inherits(label, "shiny.tag")
        is.null(buttonLabel) || is_chr1(buttonLabel)
        is.null(placeholder) || is_chr1(placeholder)
    })

    message <- list(
        id          = session$ns(inputId),
        label       = label,
        buttonLabel = buttonLabel,
        placeholder = placeholder
    )

    # Remove NULL values from message and send it.
    message <- message[!vapply(message, is.null, FUN.VALUE = NA)]
    return(session$sendCustomMessage("update_file_input", message))
}
