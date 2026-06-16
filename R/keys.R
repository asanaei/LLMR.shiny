# keys.R -----------------------------------------------------------------------
# Providers and their keys. Keys are read from environment variables only,
# following the formula c("<PROVIDER>_API_KEY", "<PROVIDER>_KEY"). No GUI built
# on this substrate ever accepts a pasted key or prints a key value.

#' Provider registry
#'
#' @return A data frame of `provider`, `display`, and `default_model`.
#' @export
provider_registry <- function() {
  data.frame(
    provider = c("groq", "openai", "anthropic", "together", "deepseek"),
    display = c("Groq", "OpenAI", "Anthropic", "Together", "DeepSeek"),
    default_model = c(
      "llama-3.3-70b-versatile",
      "gpt-4.1-mini",
      "claude-3-5-sonnet-latest",
      "meta-llama/Llama-3.3-70B-Instruct-Turbo",
      "deepseek-chat"
    ),
    stringsAsFactors = FALSE
  )
}

#' Provider choices for a select input
#'
#' @return A named character vector (`display` -> `provider`).
#' @export
provider_choices <- function() {
  reg <- provider_registry()
  stats::setNames(reg$provider, reg$display)
}

#' Default model for a provider
#'
#' @param provider Provider id.
#' @return The default model string, or `""`.
#' @export
provider_default_model <- function(provider) {
  reg <- provider_registry()
  hit <- reg$default_model[match(provider, reg$provider)]
  # match() yields NA (a length-1 NA, which %||% does not catch) for an unknown
  # provider; fall back to "" as documented so custom providers extend cleanly.
  if (length(hit) != 1L || is.na(hit)) "" else hit
}

#' Display name for a provider
#'
#' @param provider Provider id.
#' @return The display name, or the provider id.
#' @export
provider_display_name <- function(provider) {
  reg <- provider_registry()
  hit <- reg$display[match(provider, reg$provider)]
  if (length(hit) != 1L || is.na(hit)) provider else hit
}

#' Environment-variable names a provider's key may live in
#'
#' @param provider Provider id.
#' @return A length-2 character vector `c("<P>_API_KEY", "<P>_KEY")`.
#' @export
provider_env_vars <- function(provider) {
  p <- toupper(provider)
  c(paste0(p, "_API_KEY"), paste0(p, "_KEY"))
}

#' Key state for a provider, read from the environment only
#'
#' Never returns the key value; only whether one was found and in which
#' variable.
#'
#' @param provider Provider id.
#' @return A list: `provider`, `display`, `env_vars`, `found`, `env_var`.
#' @export
key_state <- function(provider) {
  provider <- provider %||% "groq"
  env_vars <- provider_env_vars(provider)
  values <- Sys.getenv(env_vars, unset = "")
  found_idx <- which(nzchar(values))[1]

  list(
    provider = provider,
    display = provider_display_name(provider),
    env_vars = env_vars,
    found = length(found_idx) == 1 && !is.na(found_idx),
    env_var = if (length(found_idx) == 1 && !is.na(found_idx)) env_vars[[found_idx]] else NA_character_
  )
}

#' A tile reporting key state (never the key value)
#'
#' @param state A [key_state()] list.
#' @return A `bslib` value box or warning card.
#' @export
key_state_tile <- function(state) {
  if (isTRUE(state$found)) {
    return(
      bslib::value_box(
        title = "API key",
        value = paste("Key found in", state$env_var),
        showcase = shiny::tags$span("Ready"),
        theme = "success"
      )
    )
  }

  bslib::card(
    class = "border-warning",
    bslib::card_header("API key not found"),
    bslib::card_body(
      shiny::tags$p(
        paste0(
          "Live run is disabled for ", state$display, ". Set ",
          paste(state$env_vars, collapse = " or "), " in ~/.Renviron."
        )
      ),
      shiny::tags$pre(paste0(state$env_vars[[1]], "=your_key_here"))
    )
  )
}

#' A banner shown when a live run is blocked for want of a key
#'
#' @param state A [key_state()] list.
#' @return A warning card.
#' @export
live_run_blocker_ui <- function(state) {
  bslib::card(
    class = "border-warning",
    bslib::card_header("Live run disabled"),
    bslib::card_body(
      shiny::tags$p(
        paste0(
          "No key was read for ", state$display, ". Set ",
          paste(state$env_vars, collapse = " or "),
          " in ~/.Renviron and restart R."
        )
      )
    )
  )
}
