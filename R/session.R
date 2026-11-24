.dhis2_state <- new.env(parent = emptyenv())

#' Open a DHIS2 session
#'
#' Stores the server address, login credentials and request settings.
#' Later calls use this session by default. To use several servers, save
#' each session and pass it to calls through the `session` argument.
#'
#' The server address and credentials are read from arguments first, then
#' from environment variables (`DHIS2_BASE_URL`, `DHIS2_USERNAME`,
#' `DHIS2_PASSWORD`, `DHIS2_TOKEN`). Missing passwords and tokens are looked
#' up in the system keyring using entries saved by [dhis2_keyring_set()].
#'
#' @param base_url Server address, with or without a trailing `/api`.
#' @param username,password Credentials for basic authentication.
#' @param token Personal access token, sent as `Authorization: ApiToken`.
#' @param auth `"basic"` or `"token"`. When `NULL`, basic is used if a
#'   username and password are available; otherwise, token authentication
#'   is used.
#' @param api_version Optional API version, such as `40`, inserted into
#'   every path as `api/40/...`.
#' @param keyring Look in the system keyring for a missing password or
#'   token. Requires the keyring package.
#' @param set_default Make this the session later calls use.
#' @param ... Request settings for this session only; see
#'   [dhis2_defaults()].
#' @return A `dhis2_session` object, invisibly when `set_default` is `TRUE`.
#' @examples
#' \dontrun{
#' dhis2API::dhis2_session("https://play.im.dhis2.org/stable-2-41-1",
#'   username = "admin", password = "district")
#' dhis2API::dhis2_ping()
#' }
#' @export
dhis2_session <- function(base_url = NULL, username = NULL, password = NULL,
                          token = NULL, auth = NULL, api_version = NULL,
                          keyring = TRUE, set_default = TRUE, ...) {
  settings <- list(...)
  unknown <- setdiff(names(settings), names(.dhis2_defaults))
  if (length(unknown) > 0) {
    cli::cli_abort(c(
      "Unknown setting{?s}: {.val {unknown}}.",
      "i" = "See {.fn dhis2API::dhis2_defaults} for the settings."
    ))
  }

  base_url <- base_url %||% .dhis2_env_var("DHIS2_BASE_URL")
  if (is.null(base_url)) {
    cli::cli_abort(c(
      "No server address: pass {.arg base_url} or set
       {.envvar DHIS2_BASE_URL}.",
      "i" = "R reads a project {.file .Renviron} instead of the one in your
             home directory. An empty {.file .Renviron} in the working
             directory hides every key in {.file ~/.Renviron}."
    ))
  }
  base_url <- sub("/api/?$", "", sub("/+$", "", base_url))

  username <- username %||% .dhis2_env_var("DHIS2_USERNAME")
  password <- password %||% .dhis2_env_var("DHIS2_PASSWORD")
  token <- token %||% .dhis2_env_var("DHIS2_TOKEN")

  if (isTRUE(keyring) && rlang::is_installed("keyring")) {
    if (is.null(password) && !is.null(username)) {
      password <- .dhis2_keyring_get(base_url, username)
    }
    if (is.null(token)) {
      token <- .dhis2_keyring_get(base_url, "token")
    }
  }

  has_basic <- !is.null(username) && !is.null(password)
  auth <- auth %||% if (has_basic) "basic" else "token"
  auth <- rlang::arg_match(auth, c("basic", "token"))

  if (auth == "basic" && !has_basic) {
    cli::cli_abort(c(
      "Basic authentication needs both a username and a password.",
      "i" = "Set {.envvar DHIS2_USERNAME} and {.envvar DHIS2_PASSWORD}, or
             store the password with {.fn dhis2API::dhis2_keyring_set}."
    ))
  }
  if (auth == "token" && is.null(token)) {
    cli::cli_abort(c(
      "No credentials found.",
      "i" = "Set {.envvar DHIS2_USERNAME} and {.envvar DHIS2_PASSWORD} or
             {.envvar DHIS2_TOKEN}, or store them with
             {.fn dhis2API::dhis2_keyring_set}."
    ))
  }

  session <- structure(
    list(
      base_url = base_url,
      auth = auth,
      username = username,
      password = if (auth == "basic") password else NULL,
      token = if (auth == "token") token else NULL,
      api_version = api_version,
      settings = settings
    ),
    class = "dhis2_session"
  )

  if (isTRUE(set_default)) {
    .dhis2_state$session <- session
    return(invisible(session))
  }
  session
}

#' @export
format.dhis2_session <- function(x, ...) {
  who <- if (x$auth == "basic") x$username else "personal access token"
  version <- if (is.null(x$api_version)) "" else {
    paste0(" (API ", x$api_version, ")")
  }
  c(
    paste0("<dhis2_session> ", x$base_url, version),
    paste0("  auth: ", x$auth, ", as ", who),
    if (length(x$settings) > 0) {
      paste0("  settings: ", paste(names(x$settings), collapse = ", "))
    }
  )
}

#' @export
print.dhis2_session <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}

#' Get, set or clear the default session
#'
#' `dhis2_get_session()` returns the session later calls use, or `NULL`.
#' `dhis2_set_session()` makes a session the default, and
#' `dhis2_clear_session()` removes it. With no default set, each call opens
#' a session from the environment.
#'
#' @param session A session from [dhis2_session()].
#' @return The session, or `NULL`.
#' @export
dhis2_get_session <- function() {
  .dhis2_state$session
}

#' @rdname dhis2_get_session
#' @export
dhis2_set_session <- function(session) {
  .dhis2_check_session(session)
  .dhis2_state$session <- session
  invisible(session)
}

#' @rdname dhis2_get_session
#' @export
dhis2_clear_session <- function() {
  old <- .dhis2_state$session
  .dhis2_state$session <- NULL
  invisible(old)
}

# the session a call runs under: the one given, else the default, else one
# opened from the environment and not kept
.dhis2_session <- function(session = NULL) {
  if (!is.null(session)) {
    .dhis2_check_session(session)
    return(session)
  }
  .dhis2_state$session %||% dhis2_session(set_default = FALSE)
}

.dhis2_check_session <- function(session) {
  if (!inherits(session, "dhis2_session")) {
    cli::cli_abort(
      "{.arg session} must come from {.fn dhis2API::dhis2_session}."
    )
  }
  invisible(session)
}

## ---------------------------------------------------------------------------##
# Keyring ----------------------------------------------------------------------
## ---------------------------------------------------------------------------##

# one keyring service per server, so two servers keep separate entries
.dhis2_keyring_service <- function(base_url) {
  paste0("dhis2API:", sub("^https?://", "", base_url))
}

.dhis2_keyring_get <- function(base_url, username) {
  tryCatch(
    keyring::key_get(.dhis2_keyring_service(base_url), username = username),
    error = function(e) NULL
  )
}

#' Store a DHIS2 password or token in the system keyring
#'
#' Prompts for the password or token without saving it in a script or R
#' history. [dhis2_session()] retrieves it for the same server and
#' username.
#'
#' @param base_url Server address.
#' @param username The account name. Leave `NULL` with `auth = "token"`.
#' @param auth `"basic"` to store a password, `"token"` to store a personal
#'   access token.
#' @return `NULL`, invisibly.
#' @export
dhis2_keyring_set <- function(base_url, username = NULL,
                              auth = c("basic", "token")) {
  rlang::check_installed("keyring")
  auth <- rlang::arg_match(auth)
  base_url <- sub("/api/?$", "", sub("/+$", "", base_url))
  if (auth == "basic" && is.null(username)) {
    cli::cli_abort("A password entry needs {.arg username}.")
  }
  keyring::key_set(
    .dhis2_keyring_service(base_url),
    username = if (auth == "basic") username else "token",
    prompt = if (auth == "basic") "DHIS2 password: " else "DHIS2 token: "
  )
  invisible(NULL)
}
