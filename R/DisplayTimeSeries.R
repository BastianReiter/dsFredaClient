
#' DisplayTimeSeries
#'
#' Transform time series data to display-friendly format with time points as column names.
#'
#' @param TimeSeriesData \code{data.frame} - Containing data associated with points in time
#' @param TimePointFeature \code{symbol} or \code{string} - Feature in 'TimeSeriesData' that contains time points (e.g. years)
#' @param ValueFeature \code{symbol} or \code{string} - Feature in 'TimeSeriesData' that contains values
#' @param GroupingFeature \code{symbol} or \code{string} - Optional feature in 'TimeSeriesData' that contains grouping variable
#' @param IncludeMissingTimePoints \code{logical} - Indicates whether discrete time points (e.g. years) with no data should be displayed in the time series - Default: \code{FALSE}
#'
#' @return A \code{data.frame} displaying time series data in horizontal form
#' @export
#'
#' @author Bastian Reiter
DisplayTimeSeries <- function(TimeSeriesData,
                              TimePointFeature,
                              ValueFeature,
                              GroupingFeature = NULL,
                              IncludeMissingTimePoints = FALSE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(tidyr)


    TimeSeriesData <- TimeSeriesData %>%
                          select({{ GroupingFeature }},
                                 {{ TimePointFeature }},
                                 {{ ValueFeature }})

    TimeSeriesTable <- TimeSeriesData %>%
                          pivot_wider(names_from = {{ TimePointFeature }},
                                      values_from = {{ ValueFeature }})

    if (IncludeMissingTimePoints == TRUE)
    {
        AvailableYears <- as.integer(names(TimeSeriesTable)[-1])
        TimeSpan <- min(AvailableYears):max(AvailableYears)
        MissingYears <- TimeSpan[!(TimeSpan %in% AvailableYears)]

        TimeSeriesTable <- TimeSeriesTable %>%
                                mutate(!!! setNames(rep(list(NA), length(MissingYears)), MissingYears)) %>%      # Add empty columns with missing years as column names
                                select({{ GroupingFeature }},
                                       all_of(as.character(TimeSpan)))
    }

    return(TimeSeriesTable)
}
