



    # # 2 c) Summarize server-specific reports: Diagnosis Classification
    # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #
    # # Bind rows of site-specific vectors
    # DiagnosisClassification_Servers <- as_tibble(do.call(rbind, CurationReports$DiagnosisClassification))
    #
    # # Add row with column sums and add site name feature
    # DiagnosisClassificationTable <- colSums(DiagnosisClassification_Servers) %>%
    #                                     bind_rows(DiagnosisClassification_Servers) %>%
    #                                     mutate(SiteName = c("All", names(DataSources)), .before = 1)
