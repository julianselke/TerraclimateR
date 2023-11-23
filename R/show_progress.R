#' Show Loop Progress
#'
#' Print a progress bar, percentage completed, current iteration, and, optionally, estimation
#' of remaining time until completion of loop to console.
#'
#' @param i integer. The loop counter. Expected to start at 1.
#' @param n integer. The maximum loop count, e.g. length of the data.
#' @param names A character vector of length n. The names of the dimension
#' being looped over, e.g. row.names. Defaults to iteration number.
#' @param time logical. Indication of whether to print an estimate of time to
#' complete remaining iterations. See Details section for implementation.
#' @details The time estimation is implemented by calculating a time difference
#' of the current and the last k iterations where k is i-1. Values are stored
#' in a cache for estimation of remaining time. The cache is cleared when the
#' loop is finished, i.e. when \eqn{i == n}, when the the loop is not finished
#' (i.e. \eqn{i == n} was not reached) but \eqn{i == 1}, or when the
#' function is called in a different environment. However, the cache is not
#' cleared if the last call did not complete the loop and the current call
#' does not start at \eqn{i == 1}. This may be the case if the loop is
#' continued, but a falsely set \eqn{i = x | x > 1} as start value will not
#' be recognized as an error.
#' @examples
#' for (i in 1:10) {
#'   TerraclimateR:::show_progress(i, 10, time = TRUE)
#'   Sys.sleep(2)
#' }
#'
#' @import crayon
#' @importFrom rlang caller_env

show_progress <- function() {
  #> This function leverages lexical closures. The function returned by this
  #> higher order function will always have access to the environment it was
  #> created in. This allows storing objects across calls without manipulating
  #> the global or any other environment. The challenge, however, was to
  #> determine when objects in this environment should be reset (see @details).

  # convenience function
  pretty_time <- function(x) {
    if (x > 90 * 60 * 24) return(paste(round(x / (60 * 60 * 24)), "days"))
    if (x > 90 * 60) return(paste(round(x / (60 * 60)), "hours"))
    if (x > 90) return(paste(round(x / 60), "minutes"))
    return(paste(round(x), "seconds"))
  }
  # data to enclose by function
  time_cache   <- c()
  time_stamp   <- 0
  loop_count   <- 0
  env_sentinel <- rlang::caller_env()
  # sentinel is set when i == n, i.e. loop completed;
  # if loop terminates in error, cache is cleared on next call
  completed <- FALSE
  # closure to return
  clsr <- function(i, n, names = NA_character_, time = FALSE) {
    if (identical(names,  NA_character_)) names <- as.character(1:n)
    if (!is.numeric(i) || !is.numeric(n) || i <= 0 || n <= 0) {
      stop("Both arguments 'i' and 'n' must be positive integers.")
    }
    if (i > n) {
      stop("Index 'i' must not be greater than length of data 'n'.")
    }
    if (!i == as.integer(i)) {
      warning("Removing decimal places for indexing.")
      i <- as.integer(i)
    }
    if (!is.character(names)) {
      stop("Argument 'names' must be a character vector.")
    }
    # when restarting after error/abort or called in another environment
    if ((completed == FALSE & i == 1) |
        !identical(env_sentinel, rlang::caller_env())) {
      # cleanup
      time_cache   <<- c()
      time_stamp   <<- 0
      loop_count   <<- 0
      env_sentinel <<- rlang::caller_env()
    }
    loop_count <<- loop_count + 1
    if (loop_count == 1 & i != 1) stop("Index 'i' must start at 1.")
    cat("\014\n")
    extra <- nchar("||100%")
    width <- options()$width
    step  <- round(i / n * (width - extra))
    text  <- sprintf(
      "|%s%s|% 3s%%",
      crayon::bgMagenta(strrep(" ", step)),
      strrep(" ", width - step - extra),
      round(i / n * 100)
    )
    if (time == TRUE) {
      if (i == 1) {
        # message for first timediff calculation
        cat(crayon::magenta$bold("Calculating remaining time...\n"))
        time_stamp <<- Sys.time()
      } else {
        now <- Sys.time()
        loop_dur <- difftime(now, time_stamp, units = "secs")
        # smoothing over cached values
        time_cache <<- c(time_cache, loop_dur)
        time_remaining <- mean(time_cache) * ((n - i) + 1)
        cat(crayon::magenta$bold(
          "Approximately",
          pretty_time(time_remaining),
          "remaining.\n"
        ))
        # save for next iteration
        time_stamp <<- Sys.time()
      }
    }
    cat(text)
    cat("\nProcessing ", names[i], "\n")
    if (i == n) {
      # cleanup
      completed    <<- TRUE
      time_cache   <<- c()
      loop_count   <<- 0
      env_sentinel <<- rlang::caller_env()
      cat("\r\n", crayon::green$bold(">>> COMPLETED"), "\n\n")
    } else {
      completed <<- FALSE
    }
  }
  return(clsr)
}


# assign return of higher order function, i.e. function showing progress, to
# desired name; roxygen documents the higher order function (first function
# in file)
show_progress <- show_progress()

