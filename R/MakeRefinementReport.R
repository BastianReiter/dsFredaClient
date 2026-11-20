
#' MakeRefinementReport
#'
#' Create html output based on Refinement report objects supplied by \code{\link{GetRefinementReport}}. Requires Quarto.
#'
#' @param RefinementReportData List | Refinement output object supplied by \code{\link{GetRefinementReport}}
#' @param PathToReportTemplate String | Path to a qmd-file serving as template for a Quarto document to be rendered
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MakeRefinementReport <- function(RefinementReportData,
                               PathToReportTemplate = "./Development/Reporting/RefinementReport.qmd")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    # require(quarto)
    # require(stringr)
    # require(utils)

    # For testing purposes
    # RefinementReportData <- RefinementReports$SiteA
    # PathToReportTemplate <- "./Development/Reporting/RefinementReport.qmd"

    # Temporarily save R object containing Refinement report data to make it accessible to the Quarto background R sesssion
    # saveRDS(object = RefinementReportData,
    #         file = "./Development/Reporting/RefinementReport.rds")
    #
    # tryCatch({
    #             quarto_render(input = PathToReportTemplate,
    #                           execute_params = list(PathToRefinementReportData = "RefinementReport.rds"))
    #             CompletedSuccessfully <- TRUE
    #             cat("Report created successfully!\n")
    #          },
    #          error = function(Error)
    #          {
    #             PathToRenderedReport <- NULL
    #             CompletedSuccessfully <- FALSE
    #             cat("Error rendering Quarto document:", conditionMessage(Error), "\n")
    #          })
    #
    # if (CompletedSuccessfully == TRUE)
    # {
    #     PathToRenderedReport <- str_replace(PathToReportTemplate, ".qmd", ".html")
    #     browseURL(PathToRenderedReport)
    # }

}
