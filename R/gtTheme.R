
#' gtTheme
#'
#' Custom gt theme
#'
#' Can be applied to gt table objects from gt package
#'
#' @param gtObject A gt table object
#' @param Fontname The name of the font to be used - Default: "sans" / If package \code{sysfonts} is installed, the Google font 'Karla' is chosen
#' @param ShowNAs logical; whether NA values should be printed
#' @param TableWidth \code{numeric}
#' @param TableAlign \code{string} - One of "center" / "left" / "right"
#' @param ... \code{list} of arguments
#'
#' @return A (modified) gt table object
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
gtTheme <- function(gtObject,
                    Fontname = "sans",
                    ShowNAs = FALSE,
                    TableWidth = NULL,
                    TableAlign = "center",
                    BaseFontSize = "80%",
                    ...)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
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

  gtObject <- gtObject %>%
                  gt::tab_options(table.width = TableWidth,
                                  table.align = TableAlign,
                                  table.font.names = c(Fontname, gt::default_fonts()),
                                  table.font.size = BaseFontSize,
                                  heading.align = "center",
                                  table.border.top.width = NULL,
                                  column_labels.background.color = FredaColors$PrimaryLight,
                                  column_labels.border.top.width = NULL,
                                  column_labels.border.bottom.width = 3,
                                  column_labels.border.bottom.color = FredaColors$Primary,
                                  row_group.background.color = FredaColors$LightGrey) %>%
                  #--- Style column label text ---
                  gt::tab_style(locations = cells_column_labels(),
                                style = paste0("vertical-align: middle;
                                                text-transform: uppercase;
                                                font-weight: bold;
                                                color: ", FredaColors$Primary)) %>%
                  #--- Format column label text (Replace "_" with " ")
                  gt::text_replace(locations = cells_column_labels(),
                                   pattern = "[_]",
                                   replacement = " ") %>%
                  #--- Style row group label ---
                  gt::tab_style(locations = cells_row_groups(),
                                style = paste0("font-weight: bold; color: ", FredaColors$Primary)) %>%
                  { if (ShowNAs == FALSE)
                  { gt::sub_missing(., missing_text = "") }
                    else {.}
                  }

  return(gtObject)
}
