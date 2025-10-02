
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# - DOCUMENTATION of Package Data -
#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FredaAlphaPalettes.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Alpha palettes used in Freda packages
#'
#' A list of named vectors containing alpha values
#'
#' @format ## `FredaAlphaPalettes`
#' A list of named vectors
#' \describe{
#'   \item{name}{Palette name}
#'   \item{values}{Vector of alpha values}
#' }
#' @source Own preferences
#' @author Bastian Reiter
"FredaAlphaPalettes"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FredaColors.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Preferred colors in Freda packages
#'
#' A list of named hexadecimal RGB codes
#'
#' @format ## `FredaColors`
#' A list of named strings (hexadecimal RGB codes)
#' \describe{
#'   \item{name}{Color name}
#'   \item{code}{Color code}
#' }
#' @source Own preferences after researching a lot of color palette stuff
#' @author Bastian Reiter
"FredaColors"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ServerSpecifications.rda
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Template of a data.frame containing server-specific login credentials and project names being passed to \code{\link{ConnectToCCP}}.
#'
#' data.frame with four columns
#' \itemize{\item{ServerName}
#'          \item{URL}
#'          \item{ProjectName}
#'          \item{Token}}
#'
#' @author Bastian Reiter
"ServerSpecifications"
