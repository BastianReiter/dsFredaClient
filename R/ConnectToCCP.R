
#' ConnectToCCP
#'
#' Takes credentials of CCP servers and returns a list of DSConnection-objects. Has to be executed from within a CCP bridgehead R server session.
#'
#' @param ServerSpecifications \code{data.frame} - Login data of CCP sites
#'
#' @return A list of \code{DSConnection}-objects
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ConnectToCCP <- function(ServerSpecifications,
                         proxyurl = "http://localhost")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  require(DSI)
  require(DSOpal)
  require(httr)

  # --- For Testing Purposes ---
  # ServerSpecifications = Credentials
  # proxyurl = "http://localhost"

#-------------------------------------------------------------------------------

  # Beam settings
  httr::set_config(httr::use_proxy(url = proxyurl, port = 8062))
  httr::set_config(httr::config(ssl_verifyhost = 0L, ssl_verifypeer = 0L))

  # Create an environment
  LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

  # Append credentials of participating Sites
  for (i in 1:nrow(ServerSpecifications))
  {
      LoginBuilder$append(server = ServerSpecifications$ServerName[i],
                          url = ServerSpecifications$URL[i],
                          token = ServerSpecifications$Token[i])
  }

  # Returns a data frame of login data to CCP Sites
  LoginData <- LoginBuilder$build()

  # Perform login process and get list of DSConnection objects of all servers
  CCPConnections <- DSI::datashield.login(logins = LoginData,
                                          assign = TRUE,
                                          opts = list(ssl_verifypeer = FALSE),
                                          failSafe = TRUE)

  # Return DSConnection objects
  return(CCPConnections)
}
