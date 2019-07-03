## ================================= ##
## Functions to making MySQL Queries ##
## ================================= ##
library(DBI)
library(dplyr)

# function to kill db connection
kill_db_connections <- function (con = NULL) {
  if (!is.null(con)) dbDisconnect(con)
  
  all_connections <- dbListConnections(RMySQL::MySQL())
  if (length(all_connections) > 10) {
    for(con in all_connections) { dbDisconnect(con) }
    print(paste(length(all_connections), " connections killed."))
  }
  
}

# functions to get a data.frame from MySQL query
sql.as.df <- function(statementSQL, encoding_fields = c()
                      , strip_fields = c(), from="latin1", to = "UTF-8") {
  kill_db_connections()
  conn_args <- config::get("dataconnection")
  
  con <- dbConnect(RMySQL::MySQL()
                   , user = conn_args$uid
                   , password = conn_args$pwd
                   , dbname = conn_args$database
                   , host = conn_args$server)
  
  df <- dbGetQuery(con, statementSQL)
  kill_db_connections(con)
  
  for (field in encoding_fields) df[[field]] <- iconv(df[[field]], from=from, to=to)
  for (field in strip_fields) df[[field]] <- qdap::strip(df[[field]])
  return(df)
}

# get default parameters for SQL queries
default.sql.params <- function() {
  return(list(
    user = list(prefix = 'uu', name = 'User', id = 'UserId', table = 'user_'),
    classroom = list(prefix = 'uc', name = 'Classroom', id = 'ClassroomId', table = 'classroom'),
    grade = list(prefix = 'ug', name = 'Grade', id = 'GradeId', table = 'grade'),
    school = list(prefix = 'us', name = 'School', id = 'SchoolId', table = 'school'),
    
    resource = list(prefix = 'cr', name = 'Resource', id = 'ResourceId', table = 'resource'),
    topic = list(prefix = 'ct', name = 'Topic', id = 'TopicId', table = 'topic'),
    curriculum = list(prefix = 'cc', name = 'Curriculum', id = 'CurriculumId', table = 'curriculum'),
    domain = list(prefix = 'cd', name = 'Domain', id = 'DomainId', table = 'domain_')
  ))
}

# get MySQL SELECT clause
as.select.sql.clause <- function(key, sql.params = default.sql.params()) {
  clause <- paste0()
  
  pname <- paste0(sql.params[[key]]$prefix,'.name')
  if ('user' == key) {
    pname <- paste0("CONCAT(", sql.params[[key]]$prefix, ".name,'<',", sql.params[[key]]$prefix, ".email,'>')")
  } else if ('grade' == key) {
    pname <- paste0("CONCAT(", sql.params$school$prefix, ".name,'-',", sql.params[[key]]$prefix, ".name)")
  } else if ('classroom' == key) {
    pname <- paste0("CONCAT(", sql.params$school$prefix, ".name,'-',",sql.params$grade$prefix,".name,'-',", sql.params[[key]]$prefix, ".name)")
  }
  
  return(paste0(sql.params[[key]]$prefix, '.id AS `', sql.params[[key]]$id, '`, '
                , pname, ' AS `', sql.params[[key]]$name,'`'))
}

# get MySQL WHERE clause
as.where.sql.clause <- function(filters = list(), sql.params = default.sql.params()) {
  in_where <- ''
  for (fname in names(filters)) {
    in_where <- paste(in_where, "AND",  paste0(sql.params[[fname]]$prefix,'.id')
                      , "IN", paste0("(", paste0(filters[[fname]], collapse = ","),")"))
  }
  return(in_where)
}

