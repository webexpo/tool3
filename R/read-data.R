#' Read Measurements Dataset
#'
#' Read a measurement dataset. This must be a comma-separated values (CSV) file,
#' or a Microsoft Excel spreadsheet file (XLS or XLSX).
#'
#' CSV files are read by [data.table::fread()], XLS files by
#' [readxl::read_xls()], and XLSX files by [readxl::read_xlsx()]. See code
#' comments below for what these functions expect.
#'
#' @param path A character string. A path to the dataset to read.
#'
#' @param type A character string. The file's underlying MIME type. 3 values
#'   are supported: text/csv (.csv), application/vnd.ms-excel (.xls), and
#'   application/vnd.openxmlformats-officedocument.spreadsheetml.sheet (.xlsx).
#'
#' @returns A `data.frame`.
#'
#' @author Jean-Mathieu Potvin (<jeanmathieupotvin@@ununoctium.dev>)
#'
#' @rdname read-data
#' @export
read_data <- function(path = "", type = "") {
    stopifnot(exprs = {
        is_chr1(path)
        is_chr1(type)
    })

    if (!utils::file_test("-f", path)) {
        return(NULL)
    }

    return(
        tryCatch(condition = \(cond) { return(NULL) }, {
            switch(type,
                "text/csv" = data.table::fread(
                    file = path,

                    # Format parameters.
                    encoding = "UTF-8", # Encoding must always be UTF-8 for consistency.
                    sep      = "auto",  # Separator can be ",", "\t", "|", ";", or ":".
                    dec      = "auto",  # Separator for decimals can be ".", or ",".
                    quote    = "\"",    # Always use double quotes.
                    tz       = "UTC",   # Alays use UTC time for time and dates.

                    check.names = TRUE, # Ensure column names are valid.

                    blank.lines.skip = TRUE,  # Ignore blank lines.
                    keepLeadingZeros = FALSE, # Read numeric values ad double.
                    strip.white      = TRUE,  # Strip leading/trailing whitespaces.

                    # Types.
                    logical01 = TRUE,     # Read 0 and 1 as logical values.
                    logicalYN = TRUE,     # Read "Y" and "N" as logical values.
                    integer64 = "double", # Always read numbers as double values.

                    # Internal parameters.
                    data.table   = FALSE, # Return a data.frame.
                    showProgress = FALSE, # Do not show progress.
                    nThread      = 1L     # Keep the operation single-threaded.
                ),

                "application/vnd.ms-excel" = readxl::read_xls(
                    path,

                    sheet = NULL, # Read all ranges of first sheet.
                    range = NULL, # Idem.

                    .name_repair = "unique", # Ensure column names are valid.
                    guess_max    = 10000L,   # Ensure column is of the expected type (like data.table::fread()).

                    # Format parameters.
                    na      = "",   # Treat empty cells as NA.
                    trim_ws = TRUE, # Strip leading/trailing whitespaces.

                    # Internal parameters.
                    progress = FALSE
                ) |>
                as.data.frame(),

                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet" = readxl::read_xlsx(
                    path,

                    sheet = NULL, # Read all ranges of first sheet.
                    range = NULL, # Idem.

                    .name_repair = "unique", # Ensure column names are valid.
                    guess_max    = 10000L,   # Ensure column is of the expected type (like data.table::fread()).

                    # Format parameters.
                    na      = "",   # Treat empty cells as NA.
                    trim_ws = TRUE, # Strip leading/trailing whitespaces.

                    # Internal parameters.
                    progress = FALSE
                ) |>
                as.data.frame(),

                # Return NULL for any other type.
                # This will later be interpreted as an error.
                NULL
            )
        })
    )
}
