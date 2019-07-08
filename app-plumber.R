wants <- c('DBI','RMySQL','config','plotly','rjson','qdap')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

# comment the following line for the production environment
# NOTE: when it is remove the default data connection is used
Sys.setenv(R_CONFIG_ACTIVE = "development") 

library(plumber)
r <- plumb("api-plumber.R")
r$run(port=8080, swagger=T))
