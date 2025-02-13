#' Aggregate variables with respect to a specified level
#'
#' This function aggregates selected variables at a given categorical level and
#' replaces them with a new summarized entry in the dataset.
#'
#' @param df A data frame containing the data to be aggregated.
#' @param variables A character vector of variable names that should be aggregated.
#' @param level A string specifying the column in `df` that defines the level at which
#' aggregation is performed.
#' @param newname A string specifying the name for the newly aggregated category.
#'
#' @return A data frame where the selected variables have been aggregated into
#' a single variable with the specified `newname`.
#'
#' @importFrom dplyr filter group_by across all_of summarise mutate ungroup select
#' @importFrom rlang :=

aggToLevel <- function(df, variables, level, newname) {
  aggregatedLevel <- df %>%
    filter(.data[[level]] %in% variables) %>%
    group_by(across(-any_of(c("value", level)))) %>%
    summarise(value = sum(.data[["value"]], na.rm = TRUE), .groups = "drop") %>%
    mutate(!!level := newname) %>%
    ungroup() %>%
    select(all_of(colnames(df)))

  df %>%
    filter(!(.data[[level]] %in% variables)) %>%
    rbind(aggregatedLevel)
}
