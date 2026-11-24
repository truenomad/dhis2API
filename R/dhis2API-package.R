#' @keywords internal
"_PACKAGE"

#' @importFrom rlang %||% .data
#' @importFrom dbplyr sql
#' @importFrom utils head
NULL

# column names used inside dplyr and tidyr data masks
utils::globalVariables(c(
  "aggregation_type",
  "ancestors",
  "data_element",
  "de_name",
  "de_uid",
  "deleted",
  "element_group",
  "id",
  "last_run",
  "level",
  "level_name",
  "n",
  "name",
  "org_unit",
  "ou_uid",
  "period",
  "run_at",
  "value_type"
))
