

# Load package namespace. This also registers necessary ResourceResolvers.
library(resourcer)

# Definition of test resource, exemplary with local csv-file
TestResource.res <- resourcer::newResource(name = "TestResource",
                                           url = "file://./Development/Test/DummyData.csv",
                                           format = "csv")


# List resources at servers
DSI::datashield.resources(conns = CCPConnections)

# Status/Accessibility of specific resource
DSI::datashield.resource_status(conns = CCPConnections,
                                resource = "TestResource")


DSI::datashield.assign.resource(conns = CCPConnections,
                                symbol = "TestData",
                                resource = "TestResource")


resourcer::getResourceResolvers()


as.data.frame(TestResource.res)


TestResource.client <- resourcer::newResourceClient(TestResource.res)
class(TestResource.client)
data.frame(TestResource.client)
