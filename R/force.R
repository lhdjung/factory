
# Internal helper for `force_print()` and `force_print_all()`:
.force_args_ignore <- function(args, ignore, fn_name) {
  if (is.null(ignore)) {
    return(args)
  }
  # Throw error if `ignore` was misspecified:
  offenders <- ignore[!ignore %in% args]
  if (length(offenders) > 0) {
    stop(paste0(
      "`", offenders[1], "` is not an argument of `", fn_name, "()`."
    ))
  }
  if (length(ignore) == length(args)) {
    warning("All arguments are ignored.")
  }
  # Remove arguments the user wants to ignore:
  args[!args %in% ignore]
}


#' Print `force()` calls for copy and paste
#'
#' @description If you already wrote a function factory with many arguments "by
#'   hand" instead of using `build_factory()` or `rlang::new_function()`, call
#'   `force_print()` to print any missing `force()` calls. Copy and paste them
#'   into the part of your factory before the output function begins.
#'
#'   `force_print_all()` simply prints `force()` calls for all arguments.
#'
#'   The dots, `...`, are always ignored.
#'
#' @param fn Function factory.
#' @param ignore Optional string vector with names of arguments for which no
#'   `force()` calls should be printed.
#'
#' @details `force_print()` only works if `fn` internally calls `function()`. It
#'   will fail if the output function is constructed in any other way.
#'
#' @return No return value; called for printing.
#' @export
#'
#' @examples
#' # In this factory, only 1 in 3
#' # arguments is evaluated by `force()`:
#' fun <- function(x, y, z) {
#'   force(x)
#'   function(x, y, z) {
#'     x * y + z
#'   }
#' }
#'
#' force_print(fun)
#'
#' force_print_all(fun)
force_print <- function(fn, ignore = NULL) {

  if (!is.function(fn)) {
    stop("`fn` must be a function. It should be a function factory.")
  }

  if (is.primitive(fn)) {
    stop("`fn` must not be a primitive. It should be a function factory.")
  }

  fn_body <- as.character(body(fn))

  # For messages below:
  fn_name <- deparse(substitute(fn))
  fn_name <- paste0("`", fn_name, "()`")

  # Find the place within the function body where the output function starts:
  location_output_fn <- grep("function(", fn_body, fixed = TRUE)

  # There needs to be a call to `function()` within `fn_body`, otherwise
  # `force_print()` won't work and perhaps is not needed at all:
  if (length(location_output_fn) == 0) {
    msg_error <- paste0("Function ", fn_name, " does not ")
    msg_error <- paste0(msg_error, "include any calls to `function()`.\n")
    msg_error <- paste0(msg_error, "Are you sure it is a function factory ")
    msg_error <- paste0(msg_error, "that needs `force()` calls?")
    stop(msg_error)
  } else {
    location_output_fn <- location_output_fn[1]
  }

  # Get the argument names, ignoring the dots (and others, if `ignore` was
  # specified):
  args <- names(formals(fn))
  args <- args[args != "..."]
  args <- .force_args_ignore(args, ignore, fn_name)

  # If this happens, a warning was already thrown by `.force_args_ignore()`:
  if (length(args) == 0) {
    return(invisible(NULL))
  }

  # Determine which arguments are covered by `force()` calls; and consequently,
  # which are not:
  args_force_calls <- paste0("force(", args, ")")
  args_are_forced <- args_force_calls %in% fn_body[1:(location_output_fn - 1)]
  args_unforced <- args[!args_are_forced]

  # Prepare a message: Either all arguments are covered already...
  if (length(args_unforced) == 0) {
    cat("All arguments of", fn_name, "are covered by `force()` calls.\n")
    return(invisible(NULL))
  }

  # ...or at least some are not:
  if (length(args_unforced) == 1) {
    msg_args <- "is not covered by a `force()` call.\n"
    msg_args <- paste0("One argument (out of ", length(args), ") ", msg_args)
    msg_calls <- "this call"
  } else {
    msg_args <- "arguments are not covered by `force()` calls.\n"
    msg_args <- paste(length(args_unforced), "out of", length(args), msg_args)
    msg_calls <- "these calls"
  }

  # Print the message:
  cat(msg_args)
  cat(paste0("Consider including ", msg_calls, " in ", fn_name, "\n"))
  cat("before the start of the output function:\n\n")

  # Print the `force()` calls themselves:
  args_unforced <- paste0("force(", args_unforced, ")\n")
  cat(args_unforced, sep = "")
}



#' @rdname force_print
#' @export

force_print_all <- function(fn, ignore = NULL) {

  if (!is.function(fn)) {
    stop("`fn` must be a function. It should be a function factory.")
  }

  if (is.primitive(fn)) {
    stop("`fn` must not be a primitive. It should be a function factory.")
  }

  # Get the argument names, ignoring the dots (and others, if `ignore` was
  # specified):
  args <- names(formals(fn))
  args <- args[args != "..."]
  args <- .force_args_ignore(args, ignore, fn_name = deparse(substitute(fn)))

  # If this happens, a warning was already thrown by `.force_args_ignore()`:
  if (length(args) == 0) {
    return(invisible(NULL))
  }

  # Print `force()` calls:
  args <- paste0("force(", args, ")\n")
  cat(args, sep = "")
}

