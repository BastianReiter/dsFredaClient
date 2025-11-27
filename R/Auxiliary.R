
#===============================================================================
#   dsFredaClient Internal Auxiliary Functions
#===============================================================================


#===============================================================================
#' AddCumulativeRow
#'
#' Adds a cumulative row to a tibble/data.frame
#'
#' @param DataFrame \code{data.frame}
#' @param StringInNonNumericColumns \code{string} - Default: "All"
#' @keywords internal
#' @export
AddCumulativeRow <- function(DataFrame,
                             StringInNonNumericColumns = "All")
{
  # Get names of all non-numeric columns
  NonNumericColumns <- names(DataFrame %>% select(!where(is.numeric)))

  # Make cumulative row
  CumulativeRow <- DataFrame %>%
                      summarize(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) %>%      # Sum all non-numeric columns
                      bind_cols(setNames(rep(list(StringInNonNumericColumns), length(NonNumericColumns)), NonNumericColumns))      # Re-add non-numeric columns and assign 'StringInNonNumericColumns' as values

  # Row-bind original data.frame and cumulative row
  DataFrame %>%
      bind_rows(CumulativeRow)
}
#===============================================================================



#===============================================================================
#' CheckDSConnections
#'
#' This function checks if 'DSConnections' that are passed to a DataSHIELD client function are valid. If this object is not passed, the function will try to find an existing \code{list} programmatically (using \code{DSI} functionality) and return it.
#'
#' @param DSConnections Usually a \code{list} of \code{DSConnection} class objects. If this is set to \code{NULL} this function will try to find an existing \code{list} programmatically and return it.
#' @keywords internal
#' @export
CheckDSConnections <- function(DSConnections)
{
  # If no DataSHIELD connections are specified, assign them programmatically
  if (is.null(DSConnections))
  {
      DSConnections <- DSI::datashield.connections_find()
  }

  # If 'DSConnections' is not valid, throw an error message
  if (!(is.list(DSConnections) && all(unlist(lapply(DSConnections, function(d) { methods::is(d,"DSConnection") })))))
  {
      stop("'DSConnections' must be a list of DSConnection-class objects", call. = FALSE)
  }

  return(DSConnections)
}
#===============================================================================


#===============================================================================
#' ComputeSurvHaz
#'
#' Compute estimates (with standard errors) for cumulative survival and cumulative hazard based on timepoint-specific number at risk at number of events from a life table.
#'
#' @param n.risk \code{integer vector} - The timepoint-specific number at risk in a life table
#' @param n.event \code{integer vector} - The timepoint-specific number of events in a life table
#' @param CumHazMethod \code{string} - The method used for cumulative hazard estimation. One of 'KaplanMeier' or 'NelsonAalen' (default).
#' @export
ComputeSurvHaz <- function(n.risk,
                           n.event,
                           CumHazMethod = "NelsonAalen")
{
  # Kaplan-Meier survival estimator
  SurvEstimate <- cumprod((n.risk - n.event) / n.risk)    # Cumulative product of time-specific survival rate
  SurvStdErr <- sqrt((SurvEstimate ^ 2) * cumsum(n.event / (n.risk * (n.risk - n.event))))

  # Kaplan-Meier estimator for cumulative hazard
  CumHazEstimate <- -log(SurvEstimate)
  CumHazStdErr <- SurvStdErr / SurvEstimate

  # Nelson-Aalen estimator for cumulative hazard
  if (CumHazMethod == "NelsonAalen")
  {
      CumHazEstimate <- cumsum(n.event / n.risk)
      CumHazStdErr <- sqrt(cumsum(n.event / (n.risk ^ 2)))
  }

  return(list(SurvEstimate = SurvEstimate,
              SurvStdErr = SurvStdErr,
              CumHazEstimate = CumHazEstimate,
              CumHazStdErr = CumHazStdErr))
}
#===============================================================================


#===============================================================================
#' ComputeSurvCI
#'
#' Compute confidence interval limits for cumulative survival estimator. Can also be used for CI of cumulative hazard.
#'
#' @param SurvEstimate \code{numeric vector} - The timepoint-specific estimate for cumulative survival (Kaplan-Meier)
#' @param SurvStdError \code{integer vector} - The timepoint-specific number of events in a life table
#' @param Method \code{string} - The method used for computing confidence interval limits. One of 'NormalApproximation' or 'Cloglog' (default).
#' @param ConfLevel \code{numeric scalar} - Chosen confidence level
#' @export
ComputeSurvCI <- function(SurvEstimate,
                          SurvStdErr,
                          Method = "Cloglog",
                          ConfLevel = 0.95)
{
  # Standard way of calculating confidence interval with normal approximation
  zVal <- qnorm(ConfLevel + (1 - ConfLevel) / 2)
  CI.Lower <- SurvEstimate - zVal * SurvStdErr
  CI.Upper <- SurvEstimate + zVal * SurvStdErr

  # Alternative way using Complementary log-log link (default in survfit())
  if (Method == "Cloglog")
  {
      Theta <- log(-log(SurvEstimate))
      ThetaStdErr <- SurvStdErr / (SurvEstimate * abs(log(SurvEstimate)))
      ThetaLower <- Theta - zVal * ThetaStdErr
      ThetaUpper <- Theta + zVal * ThetaStdErr

      # Transform back (switching of upper and lower is correct)
      CI.Lower <- exp(-exp(ThetaUpper))
      CI.Upper <- exp(-exp(ThetaLower))
  }

  # Setting limits of 0 and 1 for confidence interval
  CI.Lower <- pmax(CI.Lower, 0)
  CI.Upper <- pmin(CI.Upper, 1)

  return(list(Lower = CI.Lower,
              Upper = CI.Upper))
}
#===============================================================================


#===============================================================================
#' MakeFunctionMessage
#'
#' Turn function message into named vector to enable classification of feedback
#'
#' @param Text \code{character}
#' @param IsClassInfo \code{logical}
#' @param IsClassSuccess \code{logical}
#' @param IsClassWarning \code{logical}
#' @param IsClassFailure \code{logical}
#' @keywords internal
#' @export
#-------------------------------------------------------------------------------
MakeFunctionMessage <- function(Text,
                                IsClassInfo = FALSE,
                                IsClassSuccess = FALSE,
                                IsClassWarning = FALSE,
                                IsClassFailure = FALSE)
#-------------------------------------------------------------------------------
{
  Name <- dplyr::case_when(IsClassFailure ~ "Failure",
                           IsClassWarning ~ "Warning",
                           IsClassSuccess ~ "Success",
                           IsClassInfo ~ "Info",
                           TRUE ~ "None")

  Message <- stats::setNames(object = Text,
                             nm = Name)

  return(Message)
}
#===============================================================================


#===============================================================================
#' PrintSoloMessage
#'
#' Print text passed in a one-dimensional (named) character vector. Optionally add symbols and specific formatting based on vector element name.
#'
#' @param message \code{character vector} of length 1 with optional name
#' @export
#-------------------------------------------------------------------------------
PrintSoloMessage <- function(message)
#-------------------------------------------------------------------------------
{
  if (names(message) == "Topic")
  {
      # Print topic string in bold letters (formatted with ANSI code \033...) and with horizontal line underneath
      cat("\033[1m", as.character(message), "\n", paste0(rep("~", times = stringr::str_length(as.character(message))), collapse = ""), "\033[0m", "\n", sep = "")

  } else {

      cli::cat_bullet(as.character(message),
                      bullet = dplyr::case_when(names(message) == "Info" ~ "info",
                                                names(message) == "Success" ~ "tick",
                                                names(message) == "Warning" ~ "warning",
                                                names(message) == "Failure" ~ "cross",
                                                TRUE ~ "none"),
                      bullet_col = dplyr::case_when(names(message) == "Success" ~ dsFredaClient::FredaColors$Green,
                                                    names(message) == "Warning" ~ dsFredaClient::FredaColors$Orange,
                                                    names(message) == "Failure" ~ dsFredaClient::FredaColors$Red,
                                                    TRUE ~ "black"))
  }
}


#===============================================================================
#' PrintMessages
#'
#' Take list of messages and print them with \code{PrintSoloMessage()}.
#'
#' @param Messages \code{list} List of of named vectors
#' @export
#-------------------------------------------------------------------------------
PrintMessages <- function(Messages)
#-------------------------------------------------------------------------------
{
  purrr::walk(.x = Messages,
              .f = function(Subvector)      # List of messages contains named vectors that serve as 'topic-specific messages'
                   {
                        cat("\n")

                        for (i in 1:length(Subvector))      # for-loop instead of nested purrr::walk because items in list are vectors
                        {
                            PrintSoloMessage(Subvector[i])
                        }

                        cat("\n")
                   })
}
#===============================================================================


#===============================================================================
#' Serialize
#'
#'
#'
#' @return A list containing the encoding key, with 'input' specifying the characters to be encoded
#' and 'output' specifying their corresponding encoded values.
#' @keywords internal
#' @export
#' @noRd
Serialize <- function()
{
  # encode_list <- list(input = c("(", ")", "\"", ",", " ", "!", "&", "|", "'", "=", "+", "-", "*", "/", "^", ">", "<", "~", "\n"),
  #                     output = c("$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$EXCL$", "$AND$", "$OR$",
  #                                "$APO$", "$EQU$", "$ADD$", "$SUB$", "$MULT$", "$DIVIDE$", "$POWER$", "$GT$", "$LT$", "$TILDE$", "$LINE$"))
  #
  # return(encode_list)
}



#===============================================================================
# From dsTidyverse packages!
#===============================================================================
#' .get_encode_dictionary
#'
#' Taken from dsTidyverse package. Generate an encoding key which is used for encoding and decoding strings to pass the R parser
#'
#' @return A list containing the encoding key, with 'input' specifying the characters to be encoded
#' and 'output' specifying their corresponding encoded values.
#' @keywords internal
#' @export
#' @noRd
.get_encode_dictionary <- function()
{
  encode_list <- list(input = c("(", ")", "\"", ",", " ", "!", "&", "|", "'", "=", "+", "-", "*", "/", "^", ">", "<", "~", "\n"),
                      output = c("$LB$", "$RB$", "$QUOTE$", "$COMMA$", "$SPACE$", "$EXCL$", "$AND$", "$OR$",
                                 "$APO$", "$EQU$", "$ADD$", "$SUB$", "$MULT$", "$DIVIDE$", "$POWER$", "$GT$", "$LT$", "$TILDE$", "$LINE$"))

  return(encode_list)
}


#' Encode a string using the provided encoding key.
#'
#' @param input_string The string to be encoded.
#' @param encode_key The encoding key generated by '.get_encode_dictionary()'.
#' @return The encoded string.
#' @keywords internal
#' @export
#' @noRd
.encode_tidy_eval <- function(input_string, encode_key)
{
  encode_vec <- encode_key$output
  names(encode_vec) <- encode_key$input
  split_string <- strsplit(input_string, "")[[1]]
  output_string <- sapply(split_string,
                          function(char)
                          {
                              if (char %in% names(encode_vec))
                              { encode_vec[[char]]
                              } else { char }
                          })
  return(paste(output_string, collapse = ""))
}


#' Decode a string using the provided encoding key.
#'
#' @param input_string The encoded string passed through the R parser.
#' @param encode_key The encoding key generated by '.get_encode_dictionary()'.
#' @return The decoded string.
#' @keywords internal
#' @export
#' @noRd
.decode_tidy_eval <- function(input_string, encode_key)
{
  encode_vec <- encode_key$input
  names(encode_vec) <- encode_key$output

  output_string <- Reduce(function(out, pattern)
                          {
                              gsub(pattern, encode_vec[[pattern]], out, fixed = TRUE)
                          }, names(encode_vec), input_string)
  return(output_string)
}
#===============================================================================


#===============================================================================
# Custom Infix Operator %notin%
#' @noRd
#' @export
'%notin%' <- function(x, y) { !(x %in% y) }
#===============================================================================

