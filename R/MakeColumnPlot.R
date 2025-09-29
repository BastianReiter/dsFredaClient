
#' MakeColumn_Plot
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
#' @param FacetArguments A list
#' @param AxisType_y Optional: "absolute" (default) / "proportional"
#' @param CoordFlip
#' @param LegendPosition Optional: "top" / "bottom" / "left" / "none"
#' @param LegendShowFillGuide Optional logical: Show / hide legend guide of fill mapping
#' @param AxisTitle_x
#' @param AxisTitle_y
#' @param TickLabelWidth_x
#' @param Decimals
#' @param ggTheme
#' @param ThemeArguments Pass custom theme arguments as list object
#' @param FillPalette Custom color palette passed into function environment
#' @param ColorPrimary
#' @param AlphaPalette
#' @param ColumnWidth
#' @param ... Anonymous additional arguments
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples
#' @author Bastian Reiter
MakeColumnPlot <- function(DataFrame,
                           XFeature,
                           XFeatureSpecs = NULL,
                           XFeatureAdditionalMapping = "none",
                           YFeature,
                           GroupingFeature = NULL,
                           GroupingSpecs = NULL,
                           GroupingPosition = ggplot2::position_stack(),
                           GroupingMapping = "fill",
                           FacetFeature = NULL,
                           FacetSpecs = NULL,
                           FacetMapping = "none",
                           FacetArguments = list(),
                           AxisType_y = "absolute",
                           CoordFlip = FALSE,
                           LegendPosition = "right",
                           LegendShowFillGuide = TRUE,
                           AxisTitle_x = "",
                           AxisTitle_y = "",
                           TickLabelWidth_x = 10,
                           Decimals = 0,
                           ggTheme = function(...) dsCCPhosClient::ggTheme_CCP(...),
                           ThemeArguments = list(),
                           FillPalette = NULL,
                           ColorPrimary = dsCCPhosClient::CCPhosColors$Primary,
                           AlphaPalette = NULL,
                           ColumnWidth = 0.95,
                           ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(ggplot2)
    require(rlang)
    require(scales)


    #--- Process plot data -----------------------------------------------------

    df_Plotdata <- DataFrame %>%
                      ungroup() %>%
                      select({{ XFeature }},
                             {{ GroupingFeature }},
                             {{ FacetFeature }},
                             {{ YFeature }}) %>%
                      rename(X = {{ XFeature }},      # Rename columns for easier processing
                             Y = {{ YFeature }},
                             GroupingFeature = {{ GroupingFeature }},
                             FacetFeature = {{ FacetFeature }}) %>%
                      { if (quo_is_null(enquo(GroupingFeature)) == FALSE)      # If GroupingFeature is not empty...
                        { mutate(., GroupingFeature = factor(GroupingFeature)) }      # ...make it a factor (discrete) variable for correct processing in ggplot function, especially scale_-layers
                        else {.}
                      } %>%
                      { if (quo_is_null(enquo(FacetFeature)) == FALSE)
                        { mutate(., FacetFeature = factor(FacetFeature)) }
                        else {.}
                      }

    #--- Initialize internal objects -------------------------------------------
    var_Geom <- ggplot2::geom_col()
    var_Modifications <- list()       # Default: No modifications
    var_LegendLabels <- ggplot2::waiver()      # If no grouping specifications are passed, take legend labels from computed transformation object
    var_XLabels <- NULL

    #--- Option: Determine x value selection and order -------------------------
    if (is.null(XFeatureSpecs) == FALSE)
    {
        if (is.null(names(XFeatureSpecs)) == TRUE) { var_XLabels = XFeatureSpecs }      # If X specs are passed without vector naming, take X labels from vector values
        else { var_XLabels <- names(XFeatureSpecs) }      # Else take X labels from X specs vector names

        df_Plotdata <- df_Plotdata %>%
                            mutate(X = factor(X, levels = XFeatureSpecs, labels = var_XLabels)) %>%
                            filter(is.na(X) == FALSE)
    }


    #--- Option: Determine group level selection and order ---------------------
    if (is.null(GroupingSpecs) == FALSE)
    {
        df_Plotdata <- df_Plotdata %>%
                            mutate(GroupingFeature = factor(GroupingFeature, levels = GroupingSpecs)) %>%
                            filter(is.na(GroupingFeature) == FALSE)

        vc_RepresentedGroupValues <- unique(df_Plotdata$GroupingFeature)
        vc_RepresentedGroupingSpecs <- GroupingSpecs[GroupingSpecs %in% vc_RepresentedGroupValues]

        if (is.null(names(vc_RepresentedGroupingSpecs)) == TRUE) { var_LegendLabels = vc_RepresentedGroupingSpecs }      # If grouping specs are passed without vector naming, take legend labels from vector values
        else { var_LegendLabels <- names(vc_RepresentedGroupingSpecs) }      # Else take legend labels from grouping specs vector names
    }


    #--- Option: Determine facet level selection and order ---------------------
    if (is.null(FacetSpecs) == FALSE)
    {
        df_Plotdata <- df_Plotdata %>%
                            mutate(FacetFeature = factor(FacetFeature, levels = FacetSpecs)) %>%
                            filter(is.na(FacetFeature) == FALSE)
    }


    var_FillMapping <- ColorPrimary      # Default: Fill color taken from ColorPrimary
    var_AlphaMapping <- "static"      # Default: Consistent, if no alpha mapping
    var_AlphaValues <- 0.8      # Default alpha value, if no alpha mapping
    var_PatternMapping <- NULL

    if (XFeatureAdditionalMapping == "fill") { var_FillMapping <- df_Plotdata$X }
    if (XFeatureAdditionalMapping == "alpha") { var_AlphaMapping <- df_Plotdata$X }
    if (XFeatureAdditionalMapping == "pattern") { var_PatternMapping <- df_Plotdata$X }

    if (quo_is_null(enquo(GroupingFeature)) == FALSE)
    {
        if (GroupingMapping == "fill") { var_FillMapping <- df_Plotdata$GroupingFeature }
        if (GroupingMapping == "alpha") { var_AlphaMapping <- df_Plotdata$GroupingFeature }
        if (GroupingMapping == "pattern") { var_PatternMapping <- df_Plotdata$GroupingFeature }
    }

    if (quo_is_null(enquo(FacetFeature)) == FALSE)
    {
      if (FacetMapping == "fill") { var_FillMapping <- df_Plotdata$FacetFeature }
      if (FacetMapping == "alpha") { var_AlphaMapping <- df_Plotdata$FacetFeature }
      if (FacetMapping == "pattern") { var_PatternMapping <- df_Plotdata$FacetFeature }
    }


    #--- Set aesthetics mapping ------------------------------------------------
    var_aesMapping <- aes(fill = var_FillMapping,
                          alpha = var_AlphaMapping)

    #--- Determine geom() object -----------------------------------------------
    var_Geom <- geom_col(mapping = var_aesMapping,
                         position = GroupingPosition,
                         width = ColumnWidth)


    #--- If fill mapping is set to X or Facet Feature --------------------------
    if (XFeatureAdditionalMapping == "fill" | FacetMapping == "fill")
    {
        var_Modifications <- c(var_Modifications,
                               list(scale_fill_manual(labels = var_LegendLabels,
                                                      values = FillPalette,      # Set custom fill colors
                                                      guide = NULL)))      # Do not display corresponding legend
    }

    #--- For other cases of fill mapping pass the option to hide fill guide ----
    if (LegendShowFillGuide == FALSE)
    {
        var_Modifications <- c(var_Modifications,
                               list(guides(fill = "none")))
    }

    #--- If alpha mapping is enabled -------------------------------------------
    if (length(var_AlphaMapping) > 1)
    {
        # Set alpha values
        if (is.null(AlphaPalette) == TRUE) {
          var_AlphaValues <- seq(from = 0.04, to = 0.8, length.out = n_distinct(df_Plotdata$GroupingFeature))
        }
        else {
          var_AlphaValues = AlphaPalette
        }

        var_Modifications <- c(var_Modifications,
                               list(scale_alpha_manual(labels = var_LegendLabels,
                                                       values = var_AlphaValues,
                                                       name = NULL)))
    }

    #--- If pattern mapping is enabled -----------------------------------------
    if (is.null(var_PatternMapping) == FALSE)
    {
        var_aesMapping <- aes(fill = var_FillMapping,
                              alpha = var_AlphaMapping,
                              pattern = var_PatternMapping)

        var_Geom <- geom_col_pattern(mapping = var_aesMapping,
                                     position = GroupingPosition,
                                     width = ColumnWidth,
                                     pattern_color = color_DarkGrey,
                                     pattern_fill = color_DarkGrey,
                                     pattern_density = 0.25,
                                     pattern_spacing = 0.04,
                                     pattern_alpha = var_PatternAlpha,
                                     pattern_key_scale_factor = 0.5)

        var_Modifications <- c(var_Modifications,
                               list(scale_pattern_manual(labels = var_LegendLabels,
                                                         values = c("stripe", "crosshatch", "stripe"),
                                                         name = NULL)))
    }

    #--- Option: Format y axis for display of proportional values --------------
    if (AxisType_y == "proportional")
    {
      # List of ggplot layer objects to modify y axis according to proportional values
      var_Modifications <- c(var_Modifications,
                             list(scale_y_continuous(labels = function(x) paste(round(x * 100, 0), "%"),      # Format y axis tick mark labels: Percent
                                                     expand = expansion(mult = c(0, 0.1))),      # No padding between data lower y limit, 10 % padding on upper y limit
                                  theme(axis.line.y = element_line(arrow = NULL))))
    }


    #--- Plot ------------------------------------------------------------------

    plot <- ggplot(data = df_Plotdata,
                   aes(x = X,
                       y = Y)) +
            do.call(ggTheme, c(list(...),      # Apply custom theme with optional arguments as concatenated lists
                               list(LegendPosition = LegendPosition),      # Because legend position is often used, it gets its own argument
                               ThemeArguments)) +
            var_Geom +
            labs(x = AxisTitle_x,
                 y = AxisTitle_y) +
            #--- Option: If no axis title, delete space for label --------------
            {
              if (AxisTitle_x == "") { theme(axis.title.x = element_blank()) }
            } + {
              if (AxisTitle_y == "") { theme(axis.title.y = element_blank()) }
            } +
            # If x axis variable is not numeric: Set width of x axis tick mark labels, after which linebreak should occur
            {
              if (is.numeric(df_Plotdata$X) == FALSE) { scale_x_discrete(labels = label_wrap(TickLabelWidth_x)) }
            } +
            scale_y_continuous(labels = function(value) round(value, Decimals),      # Format y axis tick mark labels: rounded numbers
                               expand = expansion(mult = c(0, 0.1))) +      # No padding between data lower y limit, 10 % padding on upper y limit
            {
              if (!is.null(FillPalette)) { scale_fill_manual(name = NULL,
                                                             labels = var_LegendLabels,
                                                             values = FillPalette) }      # Set custom fill colors
              else { scale_color_brewer(name = NULL,
                                        labels = var_LegendLabels,
                                        type = "qual") }
            } +
            scale_alpha_manual(values = var_AlphaValues,
                               guide = NULL) +      # Do not display alpha related legend, if alpha mapping is not used
            var_Modifications +      # Pass list of optional modifications determined above
            #--- Option: Flip Coordination System ------------------------------
            {
              if (CoordFlip == TRUE) { coord_flip() }
            } +
            #--- Option: Facet -------------------------------------------------
            {
              if (quo_is_null(enquo(FacetFeature)) == FALSE)      # Check if FacetFeature is empty (after defusing FacetFeature with enquo)
              {
                  do.call(facet_wrap, c(list(facets = vars(FacetFeature)),
                                        FacetArguments))      # Use additional facet arguments via ...-Operator
              }
            }
}
