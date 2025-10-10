
#' MakeCurationReport
#'
#' Create html output based on curation report objects supplied by \code{\link{GetCurationReport}}. Requires Quarto.
#'
#' @param CurationReportData List | Curation output object supplied by \code{\link{GetCurationReport}}
#' @param PathToReportTemplate String | Path to a qmd-file serving as template for a Quarto document to be rendered
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MakeCurationReport <- function(CurationReportData,
                               PathToReportTemplate = "./Development/Reporting/CurationReport.qmd")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    # require(quarto)
    # require(stringr)
    # require(utils)

    # For testing purposes
    # CurationReportData <- CurationReports$SiteA
    # PathToReportTemplate <- "./Development/Reporting/CurationReport.qmd"

    # Temporarily save R object containing curation report data to make it accessible to the Quarto background R sesssion
    # saveRDS(object = CurationReportData,
    #         file = "./Development/Reporting/CurationReport.rds")
    #
    # tryCatch({
    #             quarto_render(input = PathToReportTemplate,
    #                           execute_params = list(PathToCurationReportData = "CurationReport.rds"))
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
