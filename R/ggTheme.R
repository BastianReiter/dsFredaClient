
#' ggTheme
#'
#' Custom ggplot2 theme, based on ggplot2::theme_classic()
#'
#' @param ... \code{list} of arguments
#' @param BaseSize Font size that relatively determines all other sizes
#' @param LegendPosition Legend position ("none", "left", "right", "bottom", "top", or two-element numeric vector)
#' @param Fontname The name of the font to be used - Default: "sans" / If package 'sysfonts' is installed, the Google font 'Karla' is chosen
#' @param SizeFactorPlotTitle Determines font size of plot title
#' @param SizeFactorPlotSubtitle Determines font size of plot subtitle
#' @param SizeFactorPlotCaption Determines font size of plot caption
#' @param SizeFactorAxisLabels Determines font size of main axis labels
#' @param SizeFactorTickLabels_x Determines font size of x axis tick labels
#' @param SizeFactorTickLabels_y Determines font size of y axis tick labels
#' @param SizeFactorLegendLabels Determines font size of legend labels
#' @param SizeFactorFacetLabels Determines font size of facet labels
#'
#' @return A \code{ggplot2::theme} object
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ggTheme <- function(...,
                    BaseSize = 11,
                    Fontname = "sans",
                    LegendPosition = "right",
                    SizeFactorPlotTitle = 1.67,
                    SizeFactorPlotSubtitle = 1.33,
                    SizeFactorPlotCaption = 1,
                    SizeFactorAxisLabels = 1.33,
                    SizeFactorTickLabels_x = 1,
                    SizeFactorTickLabels_y = 1,
                    SizeFactorLegendLabels = 1,
                    SizeFactorFacetLabels = 1.33)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---

#-------------------------------------------------------------------------------

  Fontname <- "sans"

  # Check if package 'sysfonts' is installed to choose Google font 'Karla'
  if (requireNamespace("sysfonts", quietly = TRUE) == TRUE)
  {
    # Add google font "Karla"
    sysfonts::font_add_google(name = "Karla", family = "Karla")
    Fontname <- "Karla"

  } else {

    cli::cat_bullet("Install package 'sysfonts' to make use of more fonts.", bullet = "info")
  }

  # Set up new theme by overriding classic theme (not modifying)
  ggplot2::theme_classic() %+replace%

      ggplot2::theme(#--- Parameters regarding entire plot ------------------------------
                     text = element_text(family = Fontname, size = BaseSize),      # Settings for all text elements
                     plot.background = element_rect(fill = "transparent", color = NA),      # Transparent plot background, no border
                     plot.margin = margin(0.5, 1, 0.5, 1, unit = "cm"),      # Margin around entire plot
                     plot.title = element_text(face = "bold", size = rel(SizeFactorPlotTitle), hjust = 0),
                     plot.title.position = "plot",
                     plot.subtitle = element_text(size = rel(SizeFactorPlotSubtitle), margin = margin(0.2, 0, 1, 0, unit = "cm"), hjust = 0),
                     plot.caption = element_text(size = rel(SizeFactorPlotCaption), margin = margin(1, 0, 0, 0, unit = "cm"), hjust = 1),
                     #--- Parameters regarding panel ------------------------------------
                     panel.background = element_rect(fill = "transparent", color = NA),      # Transparent panel background, no border
                     panel.border = element_blank(),      # No panel border
                     panel.grid.minor = element_blank(),      # Do not display minor grid lines
                     panel.grid.major.x = element_blank(),      # Do not display major grid lines of x axis
                     panel.grid.major.y =  element_line(color = FredaColors$MediumGrey),      # Color of y axis major grid lines
                     #--- Axis parameters -----------------------------------------------
                     axis.text = element_text(face = "bold", color = FredaColors$DarkGrey),      # Axis tick labels
                     axis.text.x = element_text(size = rel(SizeFactorTickLabels_x)),      # x Axis tick label size
                     axis.text.y = element_text(size = rel(SizeFactorTickLabels_y)),      # y Axis tick label size
                     axis.title = element_text(face = "bold", color = FredaColors$DarkGrey, size = rel(SizeFactorAxisLabels)),      # Axis title labels
                     axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
                     axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm"), angle =90),
                     axis.ticks = element_blank(),      # No axis tick marks
                     #axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), ends = "last", type = "open")),      # Arrow at top end of y-axis
                     axis.line.y = element_line(),
                     #--- Parameters regarding facet ------------------------------------
                     strip.background = element_rect(fill = FredaColors$LightGrey, color = "white"),      # No strip background
                     strip.text = element_text(size = rel(SizeFactorFacetLabels), face = "bold", margin = margin(5, 0, 5, 0)),      # Facet label text settings, including margin to plot area
                     panel.spacing = unit(0.4, "cm"),      # Spacing between facet panels
                     #--- Parameters regarding legend -----------------------------------
                     legend.position = LegendPosition,
                     legend.background = element_rect(fill = "transparent", color = NA),      # Transparent legend background, no border
                     legend.box.background = element_rect(fill = "transparent", color = NA),      # Transparent legend box background (with multiple legends), no border
                     legend.title = element_text(color = FredaColors$DarkGrey, size = rel(SizeFactorLegendLabels), face = "bold"),
                     legend.text = element_text(color = FredaColors$DarkGrey, size = rel(SizeFactorLegendLabels), face = "bold"),
                     legend.key.size = unit(1, "cm"),      # Size of legend symbols
                     #--- All other parameters ------------------------------------------
                     ...
                     )
}
