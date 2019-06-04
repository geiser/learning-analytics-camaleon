wants <- c('plumber', 'httpuv', 'listviewer', 'rjson')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

# remove the following line for production environment
# NOTE: when it is remove the default data connection is used
Sys.setenv(R_CONFIG_ACTIVE = "development") 

library(plumber)
r <- plumb("api-plumber.R")  # Where 'api-plumber.R' is the location of the file shown above
r$run(port=8000)
