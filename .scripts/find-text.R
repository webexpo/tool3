.find <- function(
    id               = "expostats:tool3",
    source_lang_code = "en",
    source_lang_name = "English",
    # Add entries to this argument to support more languages.
    other_lang_names = list(
        fr = "Français"
    ))
{
    path <- getOption("transltr.path")
 
    # Get the directory holding translations.
    dir <- dirname(path)
 
    # Create a new (empty) Translator object.
    tr <- transltr::translator(id = id)

    # Register native names of languages that must be supported.
    do.call(
        what = tr$set_native_languages,
        args = structure(
            c(as.list(source_lang_name), other_lang_names),
            names = c(source_lang_code, names(other_lang_names))
        )
    )
 
    # Detect existing translation files in i18n/.
    # The source language (English) never has one.
    files <- file.path(dir, sprintf("%s.txt", names(other_lang_names)))
    files <- files[utils::file_test("-f", files)]
 
    # Extract source text to translate from source scripts
    # and update the Translator object (by reference).
    transltr::find_source(tr = tr, interface = quote(translate))

    # Read existing translations and import them
    # back into the Translator object created above.
    # The latter is updated by reference.
    lapply(files, transltr::translations_read, tr = tr)
 
    # Export source text and translations.
    # This updates the contents of i18n/.
    transltr::translator_write(tr, overwrite = TRUE)
 
    # Update any previous Translator object
    # defined in the global environment.
    assign("tr", tr, globalenv())
 
    return(invisible(tr))
}
