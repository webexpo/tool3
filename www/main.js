// Define minimal custom Shiny handlers that update specific components
// of the application. Handlers are used whenever it is not possible to
// use shiny::*Output() functions in the R ui object. Values (messages)
// are considered valid (and checked by the server before).

// Update the window's title (what is displayed in a browser's tab).
Shiny.addCustomMessageHandler("update_window_title", (title) => {
  document.title = title;
  console.log("[INFO] Updating window's title.");
});

// Update the value of attribute "lang" of <html> element (the web page).
Shiny.addCustomMessageHandler("update_page_lang", (lang) => {
  document.documentElement.lang = lang;
  console.log(`[INFO] Updating attribute lang of root element to ${lang}.`);
});

// Update components of a file_input, a custom input extending
// shiny::fileInput with an update_file_input() mechanism.
Shiny.addCustomMessageHandler(
  "update_file_input",
  ({ id, label, buttonLabel, placeholder }) => {
    if (label) {
      document.getElementById(`${id}-label`).innerHTML = label;
    }

    if (buttonLabel) {
      let btn = document.getElementById(`${id}-btn-browse`);
      btn.removeChild(btn.firstChild);
      btn.insertBefore(document.createTextNode(buttonLabel), btn.firstChild);
    }

    if (placeholder) {
      document
        .getElementById(`${id}-input-filename`)
        .setAttribute("placeholder", placeholder);
    }

    console.log(`[INFO] Updating file input ${id}.`);
  }
);
