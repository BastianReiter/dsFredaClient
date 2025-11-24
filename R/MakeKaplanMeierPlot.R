
#' MakeKaplanMeierPlot
#'
#' Creates a \code{ggplot2::ggplot} object
#'
#' @param DataFrame Data frame that should have tidy format
#' @param XFeature Variable that contains X axis categories
#' @param XFeatureSpecs Optional: Character vector that determines selection and order of categories
#' @param XFeatureAdditionalMapping Optional: "fill" / "alpha" / "pattern" / "none" (default)
#' @param YFeature Variable that contains values / measurements
#' @param GroupingFeature Optional variable that contains group names
#' @param GroupingSpecs Optional: Character vector that determines three specifications of grouping: Selection and order of group levels (from top to bottom in stacked and left to right in dodged) and custom legend labeling (otherwise legend labels are taken from values of grouping variable)
#' @param GroupingPosition Optional: position_dodge() / position_fill() / position_stack() (default) / ...
#' @param GroupingMapping Optional: "alpha" / "pattern"
#' @param FacetFeature Optional variable for facet grouping
#' @param FacetSpecs Optional: Character vector that determines selection and order of facet levels
#' @param FacetMapping Optional: "fill" / "alpha" / "pattern" / "none" (default)
#' @param FacetArguments \code{list}
#' @param AxisType_y Optional: "absolute" (default) / "proportional"
#' @param CoordFlip \code{logical}
#' @param LegendPosition Optional: "top" / "bottom" / "left" / "none"
#' @param LegendShowFillGuide Optional logical: Show / hide legend guide of fill mapping
#' @param AxisTitle_x \code{string}
#' @param AxisTitle_y \code{string}
#' @param TickLabelWidth_x \code{integer scalar}
#' @param Decimals \code{integer scalar}
#' @param ggTheme \code{ggplot2::theme()} function object
#' @param ThemeArguments Optional \code{list} - Pass custom theme arguments as list object
#' @param FillPalette Custom color palette passed into function environment
#' @param ColorPrimary \code{string}
#' @param AlphaPalette \code{integer vector}
#' @param ColumnWidth \code{numeric} scalar
#' @param ... Anonymous additional arguments
#'
#' @return A \code{ggplot} object
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MakeColumnPlot <- function(DataFrame,

                           ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  # --- Argument Validation ---

#-------------------------------------------------------------------------------

  # Option 1): Try to recreate survfit() object from life table and use survminer::ggsurvplot
  #
  # LifeTable <- Test$All
  #
  # SurvfitObject <- survival::survfit(formula = Surv(time = LifeTable$time,
  #                                                   event = rep(1, length(LifeTable$time))) ~ 1,
  #                                    n.event = LifeTable$n.event,
  #                                    n.risk = LifeTable$n.risk)


  # Option 2)





  if(!is.null(group_col))
  {
    plot <- ggplot2::ggplot(LifeTable, ggplot2::aes(x = time, y = surv, color = .data[["strata"]])) +
      ggplot2::geom_step() +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Time", y = "Survival probability", color = "strata") +
      ggplot2::theme(legend.title = ggplot2::element_text(size = 10), legend.text = ggplot2::element_text(size = 10))
  } else {
    plot <- ggplot2::ggplot(surv_df, ggplot2::aes(x = time, y = surv)) +
      ggplot2::geom_step() +
      ggplot2::theme_minimal() +
      ggplot2::labs(x = "Time", y = "Survival probability")
  }


#-------------------------------------------------------------------------------
  return(Plot)
}
