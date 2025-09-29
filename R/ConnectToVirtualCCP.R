
#' ConnectToVirtualCCP
#'
#' Sets up a virtual DataSHIELD infrastructure that enables trying out real dsCCPhos functionality on test data.
#'
#' @param CCPTestData \code{Named list} of \code{data.frames} - CCP test data
#' @param NumberOfServers \code{integer} - The number of virtual servers to install
#' @param NumberOfPatientsPerServer \code{integer} - Optional value to restrict size of data set for faster testing - Default: NULL
#' @param AddedDsPackages \code{character vector} - Server-side DataSHIELD packages to be added to default (dsBase, dsCCPhos) - Default: NULL
#' @param Resources \code{Named list} of \code{resourcer::Resource} objects - Default: NULL
#' @param WorkingDirectory \code{string} - Optional custom working directory for virtual servers - Default: Hidden folder in R session's temporary directory (see \code{?DSLite::newDSLiteServer()})
#'
#' @return A \code{list} of \code{DSConnection}-objects
#' @export
#'
#' @author Bastian Reiter
ConnectToVirtualCCP <- function(CCPTestData,
                                NumberOfServers = 1,
                                NumberOfPatientsPerServer = NULL,
                                AddedDsPackages = NULL,
                                Resources = NULL,
                                WorkingDirectory = file.path(tempdir(), ".dslite"))
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
{
    require(dplyr)
    require(DSLite)
    require(DSI)
    require(DSOpal)

    #--- For testing purposes ---
    # CCPTestData <- TestData
    # NumberOfServers <- 3
    # NumberOfPatientsPerServer <- 2000
    # AddedDsPackages <- NULL
    # Resources <- list(TestResource = resourcer::newResource(name = "TestResource",
    #                                                         url = "file:///Development/Test/TestResource.csv",
    #                                                         format = "csv"))

    # Check value of NumberOfServers
    if (NumberOfServers > 26) { stop("Maximum value for 'NumberOfServers' is 26.", call. = FALSE) }

    # Determine names of virtual servers (here: ServerA, ServerB, ...)
    ServerNames <- paste0("Server", LETTERS[1:NumberOfServers])

    # Returns an environment
    LoginBuilder <- DSI::newDSLoginBuilder(.silent = FALSE)

    # Calculate auxiliary variables
    AllPatientIDs <- CCPTestData$patient$"_id"
    # AllPatientIDs <- CCPTestData$Patient$PatientID
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
        ServerTestData <- list(sample = as.data.frame(filter(CCPTestData$sample, CCPTestData$sample$"patient-id" %in% ServerPatientIDs)),
                             diagnosis = as.data.frame(filter(CCPTestData$diagnosis, CCPTestData$diagnosis$"patient-id" %in% ServerPatientIDs)),
                             GeneralPerformance = NULL,
                             histology = as.data.frame(filter(CCPTestData$histology, CCPTestData$histology$"patient-id" %in% ServerPatientIDs)),
                             metastasis = as.data.frame(filter(CCPTestData$metastasis, CCPTestData$metastasis$"patient-id" %in% ServerPatientIDs)),
                             "molecular-marker" = NULL,
                             OtherClassification = NULL,
                             patient = as.data.frame(filter(CCPTestData$patient, CCPTestData$patient$"_id" %in% ServerPatientIDs)),
                             progress = as.data.frame(filter(CCPTestData$progress, CCPTestData$progress$"patient-id" %in% ServerPatientIDs)),
                             "radiation-therapy" = as.data.frame(filter(CCPTestData$"radiation-therapy", CCPTestData$"radiation-therapy"$"patient-id" %in% ServerPatientIDs)),
                             tnm = as.data.frame(filter(CCPTestData$tnm, CCPTestData$tnm$"patient-id" %in% ServerPatientIDs)),
                             surgery = as.data.frame(filter(CCPTestData$surgery, CCPTestData$surgery$"patient-id" %in% ServerPatientIDs)),
                             "system-therapy" = as.data.frame(filter(CCPTestData$"system-therapy", CCPTestData$"system-therapy"$"patient-id" %in% ServerPatientIDs)),
                             TherapyRecommendation = NULL)

        # # Get data subsets that relate to sampled PatientIDs
        # ServerTestData <- list(BioSampling = NULL,
        #                      Diagnosis = as.data.frame(filter(CCPTestData$Diagnosis, CCPTestData$Diagnosis$PatientID %in% ServerPatientIDs)),
        #                      GeneralPerformance = NULL,
        #                      Histology = as.data.frame(filter(CCPTestData$Histology, CCPTestData$Histology$PatientID %in% ServerPatientIDs)),
        #                      Metastasis = as.data.frame(filter(CCPTestData$Metastasis, CCPTestData$Metastasis$PatientID %in% ServerPatientIDs)),
        #                      MolecularDiagnostics = NULL,
        #                      OtherClassification = NULL,
        #                      Patient = as.data.frame(filter(CCPTestData$Patient, CCPTestData$Patient$PatientID %in% ServerPatientIDs)),
        #                      Progress = as.data.frame(filter(CCPTestData$Progress, CCPTestData$Progress$PatientID %in% ServerPatientIDs)),
        #                      RadiationTherapy = as.data.frame(filter(CCPTestData$RadiationTherapy, CCPTestData$RadiationTherapy$PatientID %in% ServerPatientIDs)),
        #                      Staging = as.data.frame(filter(CCPTestData$Staging, CCPTestData$Staging$PatientID %in% ServerPatientIDs)),
        #                      Surgery = as.data.frame(filter(CCPTestData$Surgery, CCPTestData$Surgery$PatientID %in% ServerPatientIDs)),
        #                      SystemicTherapy = as.data.frame(filter(CCPTestData$SystemicTherapy, CCPTestData$SystemicTherapy$PatientID %in% ServerPatientIDs)),
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
               value = newDSLiteServer(tables = ServerTestData,
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
    CCPConnections <- DSI::datashield.login(logins = LoginData,
                                            assign = TRUE)

    return(CCPConnections)
}
