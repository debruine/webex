#' Create a fill-in-the-blank question
#'
#' @param answer The correct answer (can be a vector if there is more than one correct answer). If NULL or omitted, the input will be included in submitted data, but not checked for correctness
#' @param width Width of the input box in characters. Defaults to the length of the longest answer.
#' @param num Whether the input is numeric, in which case allow for leading zeroes to be omitted.
#' @param tol The tolerance within which numeric answers will be accepted; i.e. if \code{abs(response - true.answer) < tol}, the answer is correct (implies \code{num=TRUE}).
#' @param ignore_case Whether to ignore case (capitalization).
#' @param ignore_ws Whether to ignore whitespace.
#' @param regex Whether to use regex to match answers (concatenates all answers with `|` before matching).
#' @param name The name to record if you are saving your answers with a script
#' @details Writes html code that creates an input box widget. Call this function inline in an RMarkdown document. See the Web Exercises RMarkdown template for examples of its use in RMarkdown.
#' @examples
#' # What is 2 + 2?
#' fitb(4, num = TRUE)
#' 
#' # What was the name of the Beatles drummer?
#' fitb(c("Ringo", "Ringo Starr"), ignore_case = TRUE)
#'
#' # What is pi to three decimal places?
#' fitb(pi, num = TRUE, tol = .001)
#' @export
fitb <- function(answer = NULL, 
                 width = calculated_width, 
                 num = FALSE,
                 ignore_case = FALSE,
                 tol = NULL,
                 ignore_ws = TRUE, 
                 regex = FALSE,
                 name = NULL) {
  
  
  if(!is.null(tol)){
    num <- TRUE
  } 

  if (is.null(answer)) {
    answers <- ""
  } else {
    if (num) {
      answer2 <- strip_lzero(answer)
      answer <- union(answer, answer2)
    }
    
    # if width not set, calculate it from max length answer, up to limit of 100
    calculated_width <- min(100, max(nchar(answer)))
    
    answers <- jsonlite::toJSON(as.character(answer))
    answers <- gsub("\'", "&apos;", answers, fixed = TRUE)
  }
  
  sprintf("<input name='%s' class='%s %s %s %s' data-tol='%s' size='%s' data-answer='%s' />",
          make_name(name),
          ifelse(is.null(answer), "", "solveme"),
          ifelse(ignore_ws, "nospaces", ""),
          ifelse(ignore_case, " ignorecase", ""),
          ifelse(regex, " regex", ""),
          ifelse(!is.null(tol), tol, 0),
          width, answers)
}

#' Create a multiple-choice question
#'
#' @param opts Vector of alternatives. The correct answer is the element(s) of this vector named 'answer'. 
#' @param name The name to record if you are saving your answers with a script
#' @details Writes html code that creates an option box widget, with a single correct answer. Call this function inline in an RMarkdown document. See the Web Exercises RMarkdown template for further examples.
#' @examples
#' # How many planets orbit closer to the sun than the Earth?
#' mcq(c(1, answer = 2, 3))
#'
#' # Which actor played Luke Skywalker in the movie Star Wars?
#' mcq(c("Alec Guinness", answer = "Mark Hamill", "Harrison Ford"))
#' @export
mcq <- function(opts, name = NULL) {
  ix <- which(names(opts) == "answer")
  if (length(ix) == 0) {
    stop("MCQ has no correct answer")
  }
  answers <- jsonlite::toJSON(as.character(opts[ix]))
  answers <- gsub("\'", "&apos;", answers, fixed = TRUE)
  
  options <- paste0(" <option>",
                    paste(c("", opts), collapse = "</option> <option>"),
                    "</option>")
  paste0("<select class='solveme' name='", make_name(name), 
         "' data-answer='", answers, "'>",
         options, "</select>")
}

#' Create a true-or-false question
#'
#' @param answer Logical value TRUE or FALSE, corresponding to the correct answer.
#' @param name The name to record if you are saving your answers with a script
#' @details Writes html code that creates an option box widget with TRUE or FALSE as alternatives. Call this function inline in an RMarkdown document. See the Web Exercises RMarkdown template for further examples.
#' @examples
#' # True or False? 2 + 2 = 4
#' torf(TRUE)
#'
#' # True or False? The month of April has 31 days.
#' torf(FALSE)
#' @export
torf <- function(answer, name = NULL) {
  opts <- c("TRUE", "FALSE")
  if (answer)
    names(opts) <- c("answer", "")
  else
    names(opts) <- c("", "answer")
  mcq(opts, name) # just pass on name, don't call make_name
}

#' Create a set of checkboxes
#'
#' @param ... questions and answers in the format of "question text" = TRUE
#' @param name The name to record if you are saving your answers with a script (a single name or list of names) 
#'
#' @return a string with HTML for the checkbox group
#' @export
#'
#' @examples
checkbox <- function(..., name = NULL) {
  cb <- list(...)
  n <- length(cb)
  
  # add numeric suffix if one name is supplied for several checkboxes
  if (length(name) != n & n > 1) { 
    name <- paste0(make_name(name[1]), "_", 1:n) 
  }
  
  if (is.null(name)) {
    # handles 1-item NULL name without making extra names
    cbnames <- make_name(name) 
  } else {
    cbnames <- sapply(name, make_name)
  }
  
  txt <- list()
  for (i in 1:n) {
    cbname <- cbnames[[i]]
    cbanswer <- ifelse(cb[[i]], "TRUE", "FALSE")
    cbanswer <- jsonlite::toJSON(as.character(cbanswer))
    cbanswer <- gsub("\'", "&apos;", cbanswer, fixed = TRUE)
    question <- names(cb)[[i]]
    txt[i] <- sprintf("<input type='checkbox' class='solveme%s' id='%s' name='%s' data-answer='%s'> <label for='%s'>%s</label>",
                      ifelse(cb[[i]], "", " correct"), # set false to correct by default
                      cbname, cbname, cbanswer, cbname, question)
  }
  
  if (length(txt) > 1) {
    sprintf("<ul class='cb'>\n\t<li>%s</li>\n\t<button>Reveal Answers</button></ul>", paste(txt, collapse = "</li>\n\t<li>"))
  } else {
    sprintf("<span class='cb'>%s <button>Reveal Answers</button></span>", txt)
  }
}



#' Create button revealing hidden content
#'
#' @param button_text Text to appear on the button that reveals the hidden content.
#' @param button_class An optional css class for the button. Built-in options are "red", "orange", "yellow", "green", "blue", "purple", but you can add your own css styles with a custom class.
#' @seealso \code{unhide}
#' @details Writes HTML to create a content that is revealed by a button press. Call this function inline in an RMarkdown document. Any content appearing after this call up to an inline call to \code{unhide()} will only be revealed when the user clicks the button. See the Web Exercises RMarkdown Template for examples.
#' @examples
#' # default behavior is to generate a button that says "Solution"
#' hide()
#'
#' # or the button can display custom text
#' hide("Click here for a hint")
#' @export
hide <- function(button_text = "Solution", button_class = "") {
  sprintf("\n<div class='solution'><button class='%s'>%s</button>\n",
          button_class,
          button_text)
}

#' End hidden HTML content
#'
#' @seealso \code{hide}
#' @details Call this function inline in an RMarkdown document to mark the end of hidden content (see the Web Exercises RMarkdown Template for examples).
#' @examples
#' # just produce the closing </div> 
#' unhide()
#' @export
unhide <- function() {
  paste0("\n</div>\n")
}

#' Change webex widget style
#'
#' @param default The colour of the widgets when the correct answer is not filled in (defaults to blue).
#' @param correct The colour of the widgets when the correct answer not filled in (defaults to red).
#' @details Call this function inline in an RMarkdown document to change the default and correct colours using any valid HTML colour word (e.g., red, rgb(255,0,0), hsl(0, 100%, 50%) or #FF0000).
#' @examples
#' # change to green when correct
#' style_widgets(correct = "green")
#'
#' # yellow when unfilled, pink when correct
#' style_widgets("#FFFF00", "#FF3399")
#' @export
style_widgets <- function(default = "red", correct = "blue") {
  paste0(
    "\n<style>\n",
    "    .solveme { border-color: ", default,"; }\n",
    "    .solveme.correct { border-color: ", correct,"; }\n",
    "</style>\n\n"
  )
}

#' Display total correct
#'
#' @param elem The html element to display (e.g., div, h3, p, span)
#' @param args Optional arguments for css classes or styles
#'
#' @return A string with the html for displaying a total correct element
#' @export
#'
#' @examples
#' total_correct()     # <div  id="total_correct"></div>
#' total_correct("h3") # <h3  id="total_correct"></h3>
#' total_correct("p", "style='color: red;'")
#' total_correct("div", "class='customclass'")
total_correct <- function(elem = "span", args = "") {
  sprintf("<%s %s id=\"total_correct\"></%s>\n\n", 
              elem, args, elem)
}

#' Round up from .5
#'
#' @param x A numeric string (or number that can be converted to a string).
#' @param digits Integer indicating the number of decimal places (`round`) or significant digits (`signif`) to be used.
#' @details Implements rounding using the "round up from .5" rule, which is more conventional than the "round to even" rule implemented by R's built-in \code{\link{round}} function. This implementation was taken from \url{https://stackoverflow.com/a/12688836}.
#' @examples
#' round2(c(2, 2.5))
#' 
#' # compare to:
#' round(c(2, 2.5))
#' @export
round2 <- function(x, digits = 0) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

#' Strip leading zero from numeric string
#'
#' @param x A numeric string (or number that can be converted to a string).
#' @return A string with leading zero removed.
#' @examples
#' strip_lzero("0.05")
#' @export
strip_lzero <- function(x) {
  sub("^([+-]*)0\\.", "\\1.", x)
}
