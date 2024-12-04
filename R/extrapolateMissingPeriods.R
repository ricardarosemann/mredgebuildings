#' Extrapolate missing values beyond existing periods
#'
#' @param chunk grouped data.frame
#' @param key column holding values for extrapolation
#' @param slopeOfLast number of values for boundary regression
#'
#' @return data.frame with extrapolated column \code{key}
#'
#' @author Robin Hasse, Hagen Tockhorn

extrapolateMissingPeriods <- function(chunk, key, slopeOfLast = 5) {
  # remove NAs
  outChunk <- chunk
  chunk <- chunk[!is.na(chunk[[key]]), ]

  # not enough values available for regression
  if (nrow(chunk) < 2) {
    # constant value
    if (nrow(chunk) == 1) {
      outChunk[[key]] <- unique(chunk[[key]])
    } else {
      # no value
      outChunk[[key]] <- NA
    }
  } else {
    upperPeriod <- max(chunk$period)
    lowerPeriod <- min(chunk$period)

    form <- as.formula(paste(key, "~ period"))

    # linear regression at upper and lower end
    mUpper <- lm(form, tail(arrange(chunk, "period"), slopeOfLast))
    mLower <- lm(form, head(arrange(chunk, "period"), slopeOfLast))

    # extrapolate both ends
    outChunk[["valueUpper"]] <- predict(mUpper, newdata = outChunk["period"])
    outChunk[["valueLower"]] <- predict(mLower, newdata = outChunk["period"])

    # shift extrapolation to match last data points
    outChunk <- outChunk %>%
      mutate(valueUpper = .data[["valueUpper"]] * ifelse(
        .data[["valueUpper"]][.data[["period"]] == upperPeriod] != 0,
        .data[[key]][.data[["period"]] == upperPeriod] /
          .data[["valueUpper"]][.data[["period"]] == upperPeriod],
        1
      ),
      valueLower = .data[["valueLower"]] * ifelse(
        .data[["valueLower"]][.data[["period"]] == lowerPeriod] != 0,
        .data[[key]][.data[["period"]] == lowerPeriod] /
          .data[["valueLower"]][.data[["period"]] == lowerPeriod],
        1
      )) %>%
      mutate(!!key := ifelse(
        .data[["period"]] > max(chunk[["period"]]),
        .data[["valueUpper"]],
        ifelse(
          .data[["period"]] < min(chunk[["period"]]),
          .data[["valueLower"]],
          .data[[key]]
        )
      )) %>%
      select(-"valueUpper", -"valueLower")
  }

  return(outChunk)
}
