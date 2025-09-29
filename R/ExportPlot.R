
#' ExportPlot
#'
#' Helper function to save \code{ggplot2} objects into graphic files, based on \code{ggplot2::ggsave()}
#'
#' @param Plot A ggplot2 object
#' @param Directory Output directory
#' @param FileName Name of the created file (optional) -- Default: Take name from ggplot2 object
#' @param FileFormat File format (optional) -- Default: SVG -- Options: See ggplot2::ggsave() documentation
#' @param Width Plot Width in units passed by "Unit"
#' @param Height Plot Height in units passed by "Unit"
#' @param Unit Unit for size measures (optional) -- Default: cm
#' @param DPI Resolution in dot per inches (optional) -- Default: "print"
#' @param LegendPosition Legend position (optional) -- Default: NULL / no legend
#' @param ShowAxisTitle_x Whether to print X axis title
#' @param ShowAxisTitle_y Whether to print Y axis title
#' @param ShowAxisLabels_x Whether to print X axis labels
#' @param ShowAxisLabels_y Whether to print X axis labels
#'
#' @return No return; The function creates a plot file
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ExportPlot <- function(Plot,
                       Directory,
                       FileName = "default",
                       FileFormat = "svg",
                       Width,
                       Height,
                       Unit = "cm",
                       DPI = "print",
                       LegendPosition = NULL,
                       ShowAxisTitle_x = TRUE,
                       ShowAxisTitle_y = TRUE,
                       ShowAxisLabels_x = TRUE,
                       ShowAxisLabels_y = TRUE)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(ggplot2)

    if (FileName == "default") { FileName = deparse(substitute(Plot)) }

    FileName <- paste0(FileName, ".", FileFormat)

    if (ShowAxisTitle_x == FALSE) { Plot <- Plot + ggplot2::theme(axis.title.x = element_blank()) }
    if (ShowAxisTitle_y == FALSE) { Plot <- Plot + ggplot2::theme(axis.title.y = element_blank()) }
    if (ShowAxisLabels_x == FALSE) { Plot <- Plot + ggplot2::theme(axis.text.x = element_blank()) }
    if (ShowAxisLabels_y == FALSE) { Plot <- Plot + ggplot2::theme(axis.text.y = element_blank()) }

    if (is.null(LegendPosition) == FALSE) { Plot <- Plot + ggplot2::theme(legend.position = LegendPosition) }

    ggplot2::ggsave(plot = Plot,
                    filename = FileName,
                    path = Directory,
                    width = Width,
                    height = Height,
                    units = Unit,
                    dpi = DPI)
}
