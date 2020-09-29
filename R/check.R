#' @importFrom stats setNames
#' @importFrom utils adist
NULL

#' check assertion
#'
#' @param ... character strings
#' @param env environment
#'
#' @return `invisible(NULL)`
#' @export
check <- function(..., env = parent.frame()) {
  checks <- c(...)
  patterns <- ls(check_funs)
  patterns <- patterns[order(-nchar(patterns))]

  for(check in checks) {
    pattern_lgl <- which(unglue::unglue_detect(check, patterns))
    if(!any(pattern_lgl)){
      candidate <- patterns[which.min(adist(check, patterns))]
      stop("Unregistered pattern, did you mean to use\"",
           candidate, "\"", call. = FALSE)
    }
    pattern <- patterns[which.max(pattern_lgl)]
    args <- unglue::unglue(check, pattern)[[1]]
    cond <- do.call(check_funs[[pattern]], c(args, env))
    if(!cond) stop(check, call. = FALSE)
  }
}

#' set assertion checking functions
#'
#' @param ... assertion checking function definitions, the name should be an
#'   unglue pattern and the value the body of the function.
#'
#' @export
set_check_fun <- function(...) {
  dots <- substitute(...())
  patterns <- names(dots)
  for(i in seq_along(dots)) {
    pattern <- patterns[i]
    vars <- unlist(unglue::unglue(pattern, pattern), use.names = FALSE)
    vars <- substr(vars, 2, nchar(vars)-1)
    args <- setNames(replicate(length(vars), substitute()), vars)
    check_funs[[pattern]] <- as.function(c(args, alist(env=), dots[[i]]))
  }
  invisible()
}

#' setup check
#'
#' @export
setup <- function() {
  pf <- parent.frame()
  check <- check
  environment(check) <- pf
  assign("check", check, pf, )
  set_check_fun <- set_check_fun
  environment(set_check_fun) <- pf
  assign("set_check_fun", set_check_fun, pf)
  assign("check_funs", new.env(pf), pf)
  invisible(NULL)
}


check_funs <- new.env()

# set_check_fun(
#   "{var} must be a {type} vector." = {
#     val <- get(gsub("`", "", var),env)
#     is.atomic(val) && is(val, type)
#   },
#   "{var} must be a {type} scalar." = {
#     val <- get(gsub("`", "", var), env)
#     is.atomic(val) && is(val, type) && length(val) == 1
#   },
#   "{var} must be a {type} vector of length {length}." = {
#       val <- get(gsub("`", "", var), env)
#       is.atomic(val) && is(val, type) && length(val) == length
#     }
#   )
