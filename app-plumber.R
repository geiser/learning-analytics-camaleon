wants <- c('DBI','RMySQL','config','plotly','rjson','qdap')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

# uncomment the following line for develoment environment
# NOTE: when it is remove the default data connection is used
# Sys.setenv(R_CONFIG_ACTIVE = "development") 

library(plumber)
r <- plumb("api-plumber.R")  # Where 'api-plumber.R' is the location of the file shown above
r$run(port=8080, swagger=T))
