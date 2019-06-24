## ========================================================= ##
## Functions to get MySQL statements to making MySQL queries ##
## ========================================================= ##

# function to get resource SQL statement
get_resourceSQL <- function(simple = F) {
  statementSQL <- paste(
    "SELECT r.id AS `id`, r.DTYPE AS `dtype`,"
    , ifelse(simple
             , "CONCAT(r.DTYPE,'-id-',r.id) AS `name`,"
             , "CASE
                  WHEN r.title IS NOT NULL THEN CONCAT(r.DTYPE,'-id-',r.id,'-title-',SUBSTRING(r.title, 1, 20),'...')
                  ELSE CONCAT(r.DTYPE,'-id-',r.id,'-statement-',SUBSTRING(r.clean_statement, 1, 20),'...')
                END AS `name`,")
    , "CASE"
    , "WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=-2 THEN 'muito-facil'"
    , "WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=-1 THEN 'facil'"
    , "WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=0 THEN 'normal'"
    , "WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=1 THEN 'dificil'"
    , "WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=2 THEN 'muito-dificil'"
    , "ELSE 'undefined'"
    , "END AS `difficulty`"
    , "FROM resource r"
  )
  return(statementSQL)
}

# function to get SQL statement to user segmentation
get_userSegmentSQL <- function(key, filters = list(), in_where = '') {
  in_where <- paste(in_where, as.where.sql.clause(filters))
  statementSQL <- paste(
    "SELECT DISTINCT"
    , as.select.sql.clause(key)
    , "FROM studentenrollment se"
    , "INNER JOIN user_ uu ON uu.id = se.student_id"
    , "INNER JOIN classroom uc ON uc.id = se.classroom_id"
    , "INNER JOIN grade ug ON ug.id = uc.grade_id"
    , "INNER JOIN school us ON us.id = ug.school_id"
    , "WHERE se.enabled = 1 AND uu.gender != 'demo' AND uu.gender IS NOT NULL", in_where
  )
  return(statementSQL)
}

# function to get SQL statement to content segmentation
get_contentSegmentSQL <- function(key, filters = list(), in_where = '') {
  in_where <- paste(in_where, as.where.sql.clause(filters))
  statementSQL <- paste(
    "SELECT DISTINCT"
    , as.select.sql.clause(key)
    , "FROM resource_topic rt"
    , "INNER JOIN (", get_resourceSQL(), ") cr ON cr.id = rt.resources_id"
    , "INNER JOIN topic ct ON ct.id = rt.topics_id"
    , "INNER JOIN curriculum cc ON cc.id = ct.curriculum_id"
    , "INNER JOIN domain_ cd ON cd.id = cc.domain_id"
    , "WHERE 1 = 1", in_where
  )
  return(statementSQL)
}

# function to get percentagem (Pct) of completeness in SQL statement
get_completenessSQL <- function(fuser, fcontent, filters, tdate = 'daily', tserie = T
                                , in_where = ''
                                , endDate = as.numeric(Sys.time())
                                , startDate = as.numeric(Sys.time())-31556926
                                , sql.params = default.sql.params()) {
  
  if (tserie) {
    as_rate <- "value_"
    if ('curriculum' == fcontent) {
      in_from <- "curriculumlearninggoalhistoric lg"
      in_from <- paste(in_from, "INNER JOIN curriculum cc ON cc.id = lg.curriculum_id")
    } else if ('domain' == fcontent) {
      in_from <- "domainlearninggoalhistoric lg"
      in_from <- paste(in_from, "INNER JOIN domain_ cd ON cd.id = lg.domain_id")
    }
  } else {
    as_rate <- "lg.rate"
    if ('topic' == fcontent) {
      in_from <- "topiclearninggoal lg"
      in_from <- paste(in_from, "INNER JOIN topic ct ON ct.id = lg.topic_id")
      as_rate <- "CASE WHEN lg.percentage > 1 THEN 100 ELSE lg.percentage*100 END"
    } else if ('curriculum' == fcontent) {
      in_from <- "curriculumlearninggoal lg"
      in_from <- paste(in_from, "INNER JOIN curriculum cc ON cc.id = lg.curriculum_id")
    } else if ('domain' == fcontent) {
      in_from <- "domainlearninggoal lg"
      in_from <- paste(in_from, "INNER JOIN domain_ cd ON cd.id = lg.domain_id")
    }
  }
  
  in_where <- paste(in_where, as.where.sql.clause(filters))
  statementSQL <- paste(
    "SELECT"
    , as.select.sql.clause(fuser), ","
    , as.select.sql.clause(fcontent), ","
    , ifelse(tserie
             , ifelse(tdate == 'monthly'
                      , "UNIX_TIMESTAMP(DATE_FORMAT(lg.interactionTime,'%Y-%m-15')) AS `Date_`,"
                      , "UNIX_TIMESTAMP(DATE_FORMAT(lg.interactionTime,'%Y-%m-%d')) AS `Date_`,")
             , '')
    , as_rate, "AS `Pct`"
    , "FROM", in_from
    , "INNER JOIN user_ uu ON uu.id = lg.user_id"
    , "INNER JOIN studentenrollment se ON se.student_id = uu.id"
    , "INNER JOIN classroom uc ON uc.id = se.classroom_id"
    , "INNER JOIN grade ug ON ug.id = uc.grade_id"
    , "INNER JOIN school us ON us.id = ug.school_id"
    , "WHERE 1=1", in_where
  )
  if (tserie) {
    statementSQL <- paste(
      statementSQL
      , "AND (lg.interactionTime BETWEEN from_unixtime(",startDate,") AND from_unixtime(",endDate,"))"
      , "ORDER BY", paste0(c(sql.params[[fuser]]$id,sql.params[[fcontent]]$id,"Date_"), collapse=","), "ASC"
    )
  } else {
    statementSQL <- paste(
      statementSQL, "ORDER BY", paste0(c(sql.params[[fuser]]$id,sql.params[[fcontent]]$id), collapse=","), "ASC"
    )
  }
  return(statementSQL)
}

# function to get hits of problem solving in SQL statement
get_hitsProblemSQL <- function(fuser, fcontent, filters, tdate = 'daily', tserie = T
                               , in_where = ''
                               , endDate = as.numeric(Sys.time())
                               , startDate = as.numeric(Sys.time())-31556926
                               , sql.params = default.sql.params()) {
  
  in_where <- paste(in_where, as.where.sql.clause(filters))
  statementSQL <- paste(
    "SELECT"
    , as.select.sql.clause(fuser), ","
    , as.select.sql.clause(fcontent), ","
    , ifelse(tserie
             , ifelse(tdate == 'monthly'
                      , "UNIX_TIMESTAMP(DATE_FORMAT(ph.interactionTime,'%Y-%m-01')) AS `Date_`,"
                      , "UNIX_TIMESTAMP(DATE_FORMAT(ph.interactionTime,'%Y-%m-%d')) AS `Date_`,")
             , '')
    , "CASE WHEN ph.correctly_done=(1) THEN 1 ELSE 0 END AS `Done_`"
    , "FROM problemsolvinghistoric ph"
    , "INNER JOIN user_ uu ON uu.id = ph.user_id"
    , "INNER JOIN studentenrollment se ON se.student_id = uu.id"
    , "INNER JOIN classroom uc ON uc.id = se.classroom_id"
    , "INNER JOIN grade ug ON ug.id = uc.grade_id"
    , "INNER JOIN school us ON us.id = ug.school_id"
    , "INNER JOIN (", get_resourceSQL(), ") cr ON cr.id = ph.problem_id"
    , "INNER JOIN resource_topic rt ON rt.resources_id = cr.id"
    , "INNER JOIN topic ct ON ct.id = rt.topics_id"
    , "INNER JOIN curriculum cc ON cc.id = ct.curriculum_id"
    , "INNER JOIN domain_ cd ON cd.id = cc.domain_id"
    , "WHERE 1=1", in_where
  )
  if (tserie) {
    statementSQL <- paste(
      statementSQL
      , "AND (ph.interactionTime BETWEEN from_unixtime(",startDate,") AND from_unixtime(",endDate,"))"
      , "ORDER BY", paste0(c(sql.params[[fuser]]$id,sql.params[[fcontent]]$id,'Date_'), collapse=","), "ASC"
    )
  } else {
    statementSQL <- paste(
      statementSQL, "ORDER BY", paste0(c(sql.params[[fuser]]$id,sql.params[[fcontent]]$id), collapse=","), "ASC"
    )
  }
  return(statementSQL)
}

# function to get resource-engagement SQL statement
get_resource_engagementSQL <- function(tdate = 'daily', tserie = T) {
  statementSQL <- paste(
    "SELECT"
    , "rph.problem_id AS resource_id, rph.user_id AS user_id,"
    , ifelse(tserie
             , ifelse(tdate == 'monthly'
                      , "UNIX_TIMESTAMP(DATE_FORMAT(rph.interactionTime,'%Y-%m-01')) AS `Date_`,"
                      , "UNIX_TIMESTAMP(DATE_FORMAT(rph.interactionTime,'%Y-%m-%d')) AS `Date_`,")
             , '')
    , "rph.responseTime AS `Time_`"
    , "FROM problemsolvinghistoric rph"
    , "UNION"
    , "SELECT"
    , "rch.content_id AS resource_id, rch.user_id AS user_id,"
    , ifelse(tserie
             , ifelse(tdate == 'monthly'
                      , "UNIX_TIMESTAMP(DATE_FORMAT(rch.interactionTime,'%Y-%m-01')) AS `Date_`,"
                      , "UNIX_TIMESTAMP(DATE_FORMAT(rch.interactionTime,'%Y-%m-%d')) AS `Date_`,")
             , '')
    , "rch.viewTime AS `Time_`"
    , "FROM contentviewhistoric rch"
  )
  return(statementSQL)
}

# function to get engagement SQL statement
get_engagementSQL <- function(fuser, fcontent, filters, tdate = 'daily', tserie = T
                              , in_where = ''
                              , endDate = as.numeric(Sys.time())
                              , startDate = as.numeric(Sys.time())-31556926
                              , sql.params = default.sql.params()) {
  
  in_where <- paste(in_where, as.where.sql.clause(filters))  
  
  statementSQL <- paste(
    "SELECT"
    , as.select.sql.clause(fuser), ","
    , as.select.sql.clause(fcontent), ","
    , "hh.Date_ AS `Date_`,"
    , "hh.Time_ AS `Time_`"
    , "FROM resource_topic rt"
    , "INNER JOIN (", get_resourceSQL(), ") cr ON cr.id = rt.resources_id"
    , "INNER JOIN (", get_resource_engagementSQL(tdate = tdate, tserie = tserie), ") hh ON hh.resource_id = cr.id"
    , "INNER JOIN user_ uu ON uu.id = hh.user_id"
    , "INNER JOIN studentenrollment se ON se.student_id = uu.id"
    , "INNER JOIN classroom uc ON uc.id = se.classroom_id"
    , "INNER JOIN grade ug ON ug.id = uc.grade_id"
    , "INNER JOIN school us ON us.id = ug.school_id"
    , "INNER JOIN topic ct ON ct.id = rt.topics_id"
    , "INNER JOIN curriculum cc ON cc.id = ct.curriculum_id"
    , "INNER JOIN domain_ cd ON cd.id = cc.domain_id"
    , "WHERE 1=1", in_where, "AND (hh.Date_ BETWEEN", startDate, "AND", endDate, ")"
    , "ORDER BY", paste0(c(sql.params[[fuser]]$id,sql.params[[fcontent]]$id,"Date_"), collapse=","), "ASC"
  )
  
  return(statementSQL)
}
