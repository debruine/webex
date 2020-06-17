#' Start a form
#'
#' @param url The URL to handle the POST request with the form items, leave blank to just check answers
#'
#' @return a string with the html to start the form
#' @export
#'
form_start <- function(url = "") {
  sprintf("<form action='%s' class='webex_form hide_answers %s' method='post'>\n",
         url, ifelse(url == "", "local", "remote"))
}

#' End a form
#'
#' @param text The text on the submit button
#' @return
#' @export
form_end <- function(text = "Submit") {
  paste0("\n\n<button class='webex_submit'>", text, "</button>\n</form>\n")
}

make_name <- function(name) {
  names <- getOption("webex_names", c())
  
  if (is.null(name)) {
    name <- paste0("q_", length(names)+1)
  }
  
  while (name %in% names) {
    # no duplicate names
    name <- paste0(name, "_")
  }
  
  options(webex_names = c(names, name))
  
  name
}
