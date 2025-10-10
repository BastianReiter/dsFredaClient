
#' ConnectToVirtualNetwork
#'
#' Sets up a virtual DataSHIELD infrastructure that enables trying out real dsFreda functionality on test data.
#'
#' @param TestData \code{Named list} of \code{data.frames} - Test data
#' @param NumberOfServers \code{integer} - The number of virtual servers to install
#' @param NumberOfPatientsPerServer \code{integer} - Optional value to restrict size of data set for faster testing - Default: NULL
#' @param AddedDsPackages \code{character vector} - Server-side DataSHIELD packages to be added to default (dsBase, dsFreda) - Default: NULL
#' @param Resources \code{Named list} of \code{resourcer::Resource} objects - Default: NULL
#' @param WorkingDirectory \code{string} - Optional custom working directory for virtual servers - Default: Hidden folder in R session's temporary directory (see \code{?DSLite::newDSLiteServer()})
#'
#' @return A \code{list} of \code{DSConnection}-objects
#'
#' @export
#'
#' @author Bastian Reiter
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ConnectToVirtualNetwork <- function(TestData,
                                    NumberOfServers = 1,
                                    NumberOfPatientsPerServer = NULL,
                                    AddedDsPackages = NULL,
                                    Resources = NULL,
                                    WorkingDirectory = file.path(tempdir(), ".dslite"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
  #--- For Testing Purposes ---
  # TestData <- TestData
  # NumberOfServers <- 3
  # NumberOfPatientsPerServer <- 2000
  # AddedDsPackages <- NULL
  # Resources <- list(TestResource = resourcer::newResource(name = "TestResource",
  #                                                         url = "file:///Development/Test/TestResource.csv",
  #                                                         format = "csv"))

  # --- Argument Validation ---

#-------------------------------------------------------------------------------

  # Check value of NumberOfServers
  if (NumberOfServers > 26) { stop("Maximum value for 'NumberOfServers' is 26.", call. = FALSE) }

  # Determine names of virtual servers (here: ServerA, ServerB, ...)
  ServerNames <- paste0("Server", LETTERS[1:NumberOfServers])

  # Returns an environment
  LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

  # Calculate auxiliary variables
  AllPatientIDs <- TestData$patient$"_id"
  # AllPatientIDs <- TestData$Patient$PatientID
  CountTotalPatients <- n_distinct(AllPatientIDs)
  PatientsPerServer <- floor(CountTotalPatients / NumberOfServers)

  # Check if NumberOfPatientsPerServer has a feasible value and adopt it for PatientsPerServer
  if (!is.null(NumberOfPatientsPerServer))
  {
      if (NumberOfPatientsPerServer > PatientsPerServer)
      {
          stop(paste0("Not enough patients in test data for entered 'NumberOfPatientsPerServer'. Proposal value is equal or lower than ", PatientsPerServer, ". Alternatively reduce 'NumberOfServers'."), call. = FALSE)

      } else {

          PatientsPerServer <- NumberOfPatientsPerServer
      }
  }


  for (i in 1:NumberOfServers)
  {
      # 1) Prepare server data
      #~~~~~~~~~~~~~~~~~~~~~~~

      # Get a random sample of PatientIDs
      ServerPatientIDs <- sample(AllPatientIDs,
                               size = PatientsPerServer)

      # Get data subsets that relate to sampled PatientIDs
      ServerTestData <- list(sample = as.data.frame(filter(TestData$sample, TestData$sample$"patient-id" %in% ServerPatientIDs)),
                           diagnosis = as.data.frame(filter(TestData$diagnosis, TestData$diagnosis$"patient-id" %in% ServerPatientIDs)),
                           GeneralPerformance = NULL,
                           histology = as.data.frame(filter(TestData$histology, TestData$histology$"patient-id" %in% ServerPatientIDs)),
                           metastasis = as.data.frame(filter(TestData$metastasis, TestData$metastasis$"patient-id" %in% ServerPatientIDs)),
                           "molecular-marker" = NULL,
                           OtherClassification = NULL,
                           patient = as.data.frame(filter(TestData$patient, TestData$patient$"_id" %in% ServerPatientIDs)),
                           progress = as.data.frame(filter(TestData$progress, TestData$progress$"patient-id" %in% ServerPatientIDs)),
                           "radiation-therapy" = as.data.frame(filter(TestData$"radiation-therapy", TestData$"radiation-therapy"$"patient-id" %in% ServerPatientIDs)),
                           tnm = as.data.frame(filter(TestData$tnm, TestData$tnm$"patient-id" %in% ServerPatientIDs)),
                           surgery = as.data.frame(filter(TestData$surgery, TestData$surgery$"patient-id" %in% ServerPatientIDs)),
                           "system-therapy" = as.data.frame(filter(TestData$"system-therapy", TestData$"system-therapy"$"patient-id" %in% ServerPatientIDs)),
                           TherapyRecommendation = NULL)

      # # Get data subsets that relate to sampled PatientIDs
      # ServerTestData <- list(BioSampling = NULL,
      #                      Diagnosis = as.data.frame(filter(TestData$Diagnosis, TestData$Diagnosis$PatientID %in% ServerPatientIDs)),
      #                      GeneralPerformance = NULL,
      #                      Histology = as.data.frame(filter(TestData$Histology, TestData$Histology$PatientID %in% ServerPatientIDs)),
      #                      Metastasis = as.data.frame(filter(TestData$Metastasis, TestData$Metastasis$PatientID %in% ServerPatientIDs)),
      #                      MolecularDiagnostics = NULL,
      #                      OtherClassification = NULL,
      #                      Patient = as.data.frame(filter(TestData$Patient, TestData$Patient$PatientID %in% ServerPatientIDs)),
      #                      Progress = as.data.frame(filter(TestData$Progress, TestData$Progress$PatientID %in% ServerPatientIDs)),
      #                      RadiationTherapy = as.data.frame(filter(TestData$RadiationTherapy, TestData$RadiationTherapy$PatientID %in% ServerPatientIDs)),
      #                      Staging = as.data.frame(filter(TestData$Staging, TestData$Staging$PatientID %in% ServerPatientIDs)),
      #                      Surgery = as.data.frame(filter(TestData$Surgery, TestData$Surgery$PatientID %in% ServerPatientIDs)),
      #                      SystemicTherapy = as.data.frame(filter(TestData$SystemicTherapy, TestData$SystemicTherapy$PatientID %in% ServerPatientIDs)),
      #                      TherapyRecommendation = NULL)

      # # *** TEMPORARY fix *** until table names are clear
      # names(ServerTestData) <- c("sample",
      #                          "diagnosis",
      #                          "GeneralPerformance",
      #                          "histology",
      #                          "metastasis",
      #                          "molecular-marker",
      #                          "OtherClassification",
      #                          "patient",
      #                          "progress",
      #                          "radiation-therapy",
      #                          "tnm",
      #                          "surgery",
      #                          "system-therapy",
      #                          "TherapyRecommendation")


      # 2) Build virtual server in global environment
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      assign(x = paste0("Server_", ServerNames[i]),
             value = DSLite::newDSLiteServer(tables = ServerTestData,
                                             resources = Resources,
                                             config = DSLite::defaultDSConfiguration(include = c("dsBase",
                                                                                                 "resourcer",
                                                                                                 "dsCCPhos",
                                                                                                 AddedDsPackages)),
                                     home = WorkingDirectory),
             envir = .GlobalEnv)


      # 3) Add login data to login builder
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      LoginBuilder$append(server = ServerNames[i],
                          url = paste0("Server_", ServerNames[i]),
                          driver = "DSLiteDriver")


      # 4) Update AllPatientIDs: Delete used-up PatientIDs
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      AllPatientIDs <- AllPatientIDs[!(AllPatientIDs %in% ServerPatientIDs)]
  }


  # Returns a data.frame of login data
  LoginData <- LoginBuilder$build()

  # Get list of DSConnection objects of all servers
  DSConnections <- DSI::datashield.login(logins = LoginData,
                                         assign = TRUE)

  return(DSConnections)
}
