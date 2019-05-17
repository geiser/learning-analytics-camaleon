wants <- c('DBI','odbc','RMySQL','config', 'qdap', 'dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(DBI)
library(qdap)
library(odbc)
library(dplyr)


conn_args <- config::get("dataconnection")

# functions to get data.frame from MySQL query
get_df_from_SQL_Query <- function(
  statement, encoding_fields = c()
  , strip_fields = c(), from="latin1", to = "UTF-8") {
  kill_db_connections()
  
  if (Sys.getenv("R_CONFIG_ACTIVE") == "development") {
    con <- dbConnect(RMySQL::MySQL()
                     , user = conn_args$uid
                     , password = conn_args$pwd
                     , dbname = conn_args$database
                     , host = conn_args$server)
  } else {
    con <- dbConnect(odbc::odbc(), Driver = conn_args$driver
                     , UID    = conn_args$uid
                     , PWD    = conn_args$pwd
                     , Database = conn_args$database
                     , Server = conn_args$server
                     , Port   = conn_args$port)
  }
  
  df <- dbGetQuery(con, statement)
  kill_db_connections(con)
  
  for (field in encoding_fields) df[[field]] <- iconv(df[[field]], from=from, to=to)
  for (field in strip_fields) df[[field]] <- qdap::strip(df[[field]])
  return(df)
}

kill_db_connections <- function (con = NULL) {
  if (!is.null(con)) dbDisconnect(con)
  all_connections <- dbListConnections(RMySQL::MySQL())
  if (length(all_connections) > 10) {
    for(con in all_connections) { dbDisconnect(con) }
    print(paste(length(all_connections), " connections killed."))
  }
}

simple_resource_SQL <- "SELECT r.id AS `id`, r.DTYPE AS `dtype`,
CASE
WHEN r.title IS NOT NULL THEN CONCAT(r.DTYPE,'-id-',r.id,'-title-',SUBSTRING(r.title, 1, 20),'...')
ELSE CONCAT(r.DTYPE,'-id-',r.id,'-statement-',SUBSTRING(r.clean_statement, 1, 20),'...')
END AS `name`,
CASE
WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=-2 THEN 'muito-facil'
WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=-1 THEN 'facil'
WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=0 THEN 'normal'
WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=1 THEN 'dificil'
WHEN r.manualDifficulty IS NOT NULL AND r.manualDifficulty=2 THEN 'muito-dificil'
ELSE 'undefined'
END AS `difficulty`
FROM resource r
WHERE r.enabled = 1"

## function that return a list name and id 
get_user_segment <- function(value = "User", filter = list()) {
  in_select <- ""
  if (value == "School") {
    in_select <- "s.id as `SchoolId`, s.name as `School`"
  } else if (value == "Grade") {
    in_select <- "g.id as `GradeId`, CONCAT(s.name,'-', g.name) as `Grade`"
  } else if (value == "Classroom") {
    in_select <- "c.id as `ClassroomId`, CONCAT(s.name,'-',g.name,'-',c.name) as `Classroom`"
  } else  {
    in_select <- "u.id as `UserId`, CONCAT(u.name,'<',u.email,'>') as `User`"
  }
  
  in_where <- ""
  for (fname in unique(names(filter))) {
    ffname <- ifelse(
      any(c("school", "schoolid") == tolower(fname)), "s.id"
      , ifelse(
        any(c("grade", "gradeid") == tolower(fname)), "g.id"
        , ifelse(any(c("classroom", "classroomid") == tolower(fname)) , "c.id", "")))
    if (length(filter[[fname]]) > 0) {
      in_where <- paste(in_where, "AND", ffname, "IN"
                        , paste0("(", paste0(filter[[fname]], collapse = ","),")"))  
    }
  }
  
  df <- get_df_from_SQL_Query(
    paste0(
      "SELECT DISTINCT "
      , in_select
      , " FROM studentenrollment se
      INNER JOIN user_ u ON u.id = se.student_id
      INNER JOIN classroom c ON c.id = se.classroom_id
      INNER JOIN grade g ON g.id = c.grade_id
      INNER JOIN school s ON s.id = g.school_id
      WHERE se.enabled = 1 AND u.gender != 'demo' AND u.gender IS NOT NULL"
      , in_where
    ), c(value))
  to_return <- as.list(df[[paste0(value, "Id")]])
  names(to_return) <- df[[value]]
  return(to_return)
}

get_content_segment <- function(value = "Topic", filter = list()) {
  in_select <- ""
  if (value == "Domain") {
    in_select <- "d.id AS `DomainId`, d.name AS `Domain`"
  } else if (value == "Curriculum") {
    in_select <- "c.id AS `CurriculumId`, CONCAT(c.name) AS `Curriculum`"
  } else if (value == "Topic") {
    in_select <- "t.id AS `TopicId`, CONCAT(t.name) AS `Topic`"
  } else if (value == "ResourceType") {
    in_select <- "cr.dtype AS `ResourceTypeId`, cr.dtype AS `ResourceType`"
  } else if (value == "Resource") {
    in_select <- "cr.id AS `ResourceId`, cr.name AS `Resource`"
  }
  
  in_where <- ""
  for (fname in unique(names(filter))) {
    ffname <- ifelse(
      any(c("domain", "domainid") == tolower(fname)), "d.id"
      , ifelse(
        any(c("curriculum", "curriculumid") == tolower(fname)), "c.id"
        , ifelse(any(c("topic", "topicid") == tolower(fname)) , "t.id", "")))
    if (length(filter[[fname]]) > 0) {
      if (any(c("resourcetype", "resourcetypeid") == tolower(fname))) {
        in_where <- paste(
          in_where, "AND cr.dtype IN"
          , paste0("(",paste0(paste0("'",filter[[fname]],"'"),collapse=","),")"))
      } else {
        in_where <- paste(
          in_where, "AND", ffname, "IN"
          , paste0("(", paste0(paste0("'",filter[[fname]],"'"),collapse=","),")"))  
      }
    }
  }
  
  statementSQL <- paste(
    "SELECT DISTINCT"
    , in_select
    , "FROM resource_topic rt
    INNER JOIN (",simple_resource_SQL,") cr ON cr.id = rt.resources_id
    INNER JOIN topic t ON t.id = rt.topics_id
    INNER JOIN curriculum c ON c.id = t.curriculum_id
    INNER JOIN domain_ d ON d.id = c.domain_id
    WHERE 1 = 1"
    , in_where
  )
  
  df <- get_df_from_SQL_Query(statementSQL, c(value))
  to_return <- as.list(df[[paste0(value, "Id")]])
  names(to_return) <- df[[value]]
  return(to_return)
}



##
get_data <- function(typeData, filter = list(), option = list()) {
  schools <- c(); grades <- c(); classrooms <- c();  users <- c();
  domains <- c(); curriculums <- c(); topics <- c(); resourceTypes <- c(); resources <- c();
  
  statementSQL <- ""
  
  startDate <- NULL; endDate <- NULL;
  for (fname in unique(names(filter))) {
    if (any(c("domain", "domainid") == tolower(fname))) { domains <- unique(filter[[fname]]) }
    if (any(c("curriculum", "curriculumid") == tolower(fname))) { curriculums <- unique(filter[[fname]]) }
    if (any(c("topic", "topicid") == tolower(fname))) { topics <- unique(filter[[fname]]) }
    if (any(c("resourcetype", "resourcetypeid") == tolower(fname))) { resourceTypes <- unique(filter[[fname]]) }
    if (any(c("resource", "resourceid") == tolower(fname))) { resources <- unique(filter[[fname]]) }
    
    if (any(c("school", "schoolid") == tolower(fname))) { schools <- unique(filter[[fname]]) }
    if (any(c("grade", "gradeid") == tolower(fname))) { grades <- unique(filter[[fname]]) }
    if (any(c("classroom", "classroomid") == tolower(fname))) { classrooms <- unique(filter[[fname]]) }
    if (any(c("user", "userid") == tolower(fname))) { users <- unique(filter[[fname]]) }
    if ("startdate" == tolower(fname)) { startDate <- filter[[fname]] }
    if ("enddate" == tolower(fname)) { endDate <- filter[[fname]] }
  }
  
  if ("performance-learning" == typeData) {
    fcontent <- list(); fuser <- list();
    in_select_rate <- ""; in_where <- "";
    in_from <- ""; in_inner_join <- ""; in_order_by <- "";
    
    if ("pmc" == option$typePerformance) {
      
      in_select_content <- "";
      if (length(topics) > 0) {
        fcontent <- list(name="Topic", id="TopicId")
        in_select_content <- "ct.id AS `TopicId`, ct.name AS `Topic`"
        in_select_rate <- "CASE WHEN lg.percentage > 1 THEN 100 ELSE lg.percentage*100 END"
        in_from <- "topiclearninggoal lg"
        in_inner_join <- paste(in_inner_join, "INNER JOIN topic ct ON ct.id = lg.topic_id")
        in_where <- paste(in_where, "AND ct.id IN (",paste0(topics,collapse=","),")")
      } else if (length(curriculums) > 0) {
        fcontent <- list(name="Curriculum", id="CurriculumId")
        in_select_content <- "cc.id AS `CurriculumId`, cc.name AS `Curriculum`"
        in_select_rate <- "lg.rate"
        in_from <- "curriculumlearninggoal lg"
        in_inner_join <- paste(in_inner_join, "INNER JOIN curriculum cc ON cc.id = lg.curriculum_id")
        in_where <- paste(in_where,"AND cc.id IN (",paste0(curriculums,collapse=","),")")
      } else if (length(domains) > 0) {
        fcontent <- list(name="Domain", id="DomainId")
        in_select_content <- "cd.id AS `DomainId`, cd.name AS `Domain`"
        in_select_rate <- "lg.rate"
        in_from <- "domainlearninggoal lg"
        in_inner_join <- paste(in_inner_join, "INNER JOIN domain_ cd ON cd.id = lg.domain_id")
        in_where <- paste(in_where,"AND cd.id IN (",paste0(domains,collapse=","),")")
      }
      
      in_group_by <- "";
      in_select_user <- "";
      if (length(users) > 0) {
        fuser <- list(name="User", id="UserId")
        in_select_user <- "uu.id AS `UserId`, uu.name AS `User`"
        in_where <- paste(in_where,"AND uu.id IN (",paste0(users,collapse=","),")")
      } else {
        if (length(classrooms) > 0) {
          fuser <- list(name="Classroom", id="ClassroomId")
          in_select_user <- "uc.id AS `ClassroomId`, CONCAT(us.name,'-',ug.name,'-',uc.name) AS `Classroom`"
          in_select_rate <- paste0("AVG(",in_select_rate,")")
          in_inner_join <- paste(
            in_inner_join
            , "INNER JOIN classroom uc ON uc.id = se.classroom_id
            INNER JOIN grade ug ON ug.id = uc.grade_id
            INNER JOIN school us ON us.id = ug.school_id")
          in_where <- paste(in_where, "AND uc.id IN (",paste0(classrooms,collapse=","),")")
        } else if (length(grades) > 0) {
          fuser <- list(name="Grade", id="GradeId")
          in_select_user <- "ug.id AS `GradeId`, CONCAT(us.name,'-',ug.name) AS `Grade`"
          in_select_rate <- paste0("AVG(",in_select_rate,")")
          in_inner_join <- paste(
            in_inner_join
            , "INNER JOIN classroom uc ON uc.id = se.classroom_id
            INNER JOIN grade ug ON ug.id = uc.grade_id
            INNER JOIN school us ON us.id = ug.school_id")
          in_where <- paste(in_where, "AND ug.id IN (",paste0(grades,collapse=","),")")
        } else if (length(schools) > 0) {
          fuser <- list(name="School", id="SchoolId")
          in_select_user <- "us.id AS `SchoolId`, us.name AS `School`"
          in_select_rate <- paste0("AVG(",in_select_rate,")")
          in_inner_join <- paste(
            in_inner_join
            , "INNER JOIN classroom uc ON uc.id = se.classroom_id
            INNER JOIN grade ug ON ug.id = uc.grade_id
            INNER JOIN school us ON us.id = ug.school_id")
          in_where <- paste(in_where, "AND us.id IN (",paste0(schools,collapse=","),")")
        }
        in_group_by <- paste0("GROUP BY ",fuser$id,",",fuser$name,",",fcontent$id,",",fcontent$name)
      }
      
      statementSQL <- paste(
        "SELECT", in_select_user, ",", in_select_content, ","
        , in_select_rate, "AS `Pct`"
        , " FROM ", in_from
        , "INNER JOIN user_ uu ON uu.id = lg.user_id
        INNER JOIN studentenrollment se ON uu.id = se.student_id"
        , in_inner_join
        , "WHERE se.enabled = 1 AND uu.gender != 'demo' AND uu.gender IS NOT NULL"
        , in_where, in_group_by
        , "ORDER BY Pct DESC")
      
      df <- get_df_from_SQL_Query(statementSQL, c(fuser$name, fcontent$name))
      
      return(list(fcontent=fcontent, fuser=fuser, df=df, SQL=statementSQL
                  , display_col=c(fuser$name,fcontent$name,"Pct")))
      
    } else {
      
      in_where <- "1=1"; fcontent <- list(); fuser <- list();
      
      in_select_content <- ""
      if (length(topics) > 0) {
        fcontent <- list(name="Topic", id="TopicId")
        in_select_content <- "ct.id AS `TopicId`, ct.name AS `Topic`"
        in_where <- paste(in_where, "AND ct.id IN (",paste0(topics,collapse=","),")")
      } else if (length(curriculums) > 0) {
        fcontent <- list(name="Curriculum", id="CurriculumId")
        in_select_content <- "cc.id AS `CurriculumId`, cc.name AS `Curriculum`"
        in_where <- paste(in_where, "AND cc.id IN (",paste0(curriculums,collapse=","),")")
      } else if (length(domains) > 0) {
        fcontent <- list(name="Domain", id="DomainId")
        in_select_content <- "cd.id AS `DomainId`, cd.name AS `Domain`"
        in_where <- paste(in_where, "AND cd.id IN (",paste0(domains,collapse=","),")")
      }
      
      in_select_user <- "";
      if (length(users) > 0) {
        fuser <- list(name="User", id="UserId")
        in_select_user <- "uu.id AS `UserId`, CONCAT(uu.name,'<',uu.email,'>') AS `User`"
        in_where <- paste(in_where,"AND uu.id IN (",paste0(users,collapse=","),")")
      } else if (length(classrooms) > 0) {
        fuser <- list(name="Classroom", id="ClassroomId")
        in_select_user <- "uc.id AS `ClassroomId`, CONCAT(us.name,'-',ug.name,'-',uc.name) AS `Classroom`"
        in_where <- paste(in_where,"AND uc.id IN (",paste0(classrooms,collapse=","),")")
      } else if (length(grades) > 0) {
        fuser <- list(name="Grade", id="GradeId")
        in_select_user <- "ug.id AS `GradeId`, CONCAT(us.name,'-',ug.name) AS `Grade`"
        in_where <- paste(in_where,"AND ug.id IN (",paste0(grades,collapse=","),")")
      } else if (length(schools) > 0) {
        fuser <- list(name="School", id="SchoolId")
        in_select_user <- "us.id AS `SchoolId`, us.name AS `School`"
        in_where <- paste(in_where,"AND us.id IN (",paste0(schools,collapse=","),")")
      }
      
      statementSQL <- paste(
        "SELECT", in_select_user, ",", in_select_content, ","
        , "CASE WHEN ph.correctly_done=(1) THEN 1 ELSE 0 END AS `Done_`"
        , "FROM problemsolvinghistoric ph"
        , "INNER JOIN user_ uu ON uu.id = ph.user_id
        INNER JOIN studentenrollment se ON se.student_id = uu.id
        INNER JOIN classroom uc ON uc.id = se.classroom_id
        INNER JOIN grade ug ON ug.id = uc.grade_id
        INNER JOIN school us ON us.id = ug.school_id
        INNER JOIN resource cr ON cr.id = ph.problem_id
        INNER JOIN resource_topic rt ON rt.resources_id = cr.id
        INNER JOIN topic ct ON ct.id = rt.topics_id
        INNER JOIN curriculum cc ON cc.id = ct.curriculum_id
        INNER JOIN domain_ cd ON cd.id = cc.domain_id"
        , "WHERE", in_where
        , "ORDER BY", paste0(c(fuser$id,fcontent$id), collapse=","), "ASC"
      )
      
      print(statementSQL)
      
      df <- get_df_from_SQL_Query(statementSQL, c(fuser$name, fcontent$name))
      display_col <- c(fuser$name,fcontent$name,"Done_")
      if (nrow(df) > 0) {
        gdf <- group_by(df, uid = df[[fuser$id]], uname = df[[fuser$name]]
                        , cid = df[[fcontent$id]], cname = df[[fcontent$name]])
        gdf <- as.data.frame(summarise(gdf, sum = sum(Done_), n = n()))
        gdf[["Pct"]] <- gdf[["sum"]]/gdf[["n"]]
        if (option$typePerformance == "pme") {
          gdf[["Pct"]] <- rep(1, nrow(gdf)) - gdf[["Pct"]]
        }
        gdf[["Pct"]] <- gdf[["Pct"]]*100
        
        colnames(gdf)[colnames(gdf)=="uid"] <- fuser$id
        colnames(gdf)[colnames(gdf)=="uname"] <- fuser$name
        colnames(gdf)[colnames(gdf)=="cid"] <- fcontent$id
        colnames(gdf)[colnames(gdf)=="cname"] <- fcontent$name
        df <- gdf
        display_col <- c(fuser$name,fcontent$name,"Pct")
      }
      return(list(fcontent=fcontent, fuser=fuser, df=df, SQL=statementSQL
                  , display_col=display_col))
    }
    
  } else if ("engagement-learning:temporal-series" == typeData) {
    
    if (option$typeEngagement == "te") {
      fcontent <- list(); fuser <- list();
      in_category <- ""; in_inner_join <- ""; in_order_by <- "";
      
      in_where <- "1=1"
      in_select_time <- ""
      in_select_content <- ""
      if (length(resources) > 0) {
        fcontent <- list(name="Resource", id="ResourceId")
        in_select_content <- "cr.id AS `ResourceId`, cr.name AS `Resource`"
        in_where <- paste(in_where, "AND cr.id IN (",paste0(resources,collapse=","),")")
      } else if (length(topics) > 0) {
        fcontent <- list(name="Topic", id="TopicId")
        in_select_content <- "ct.id AS `TopicId`, ct.name AS `Topic`"
        in_where <- paste(in_where, "AND ct.id IN (",paste0(topics,collapse=","),")")
      } else if (length(curriculums) > 0) {
        fcontent <- list(name="Curriculum", id="CurriculumId")
        in_select_content <- "cc.id AS `CurriculumId`, cc.name AS `Curriculum`"
        in_where <- paste(in_where, "AND cc.id IN (",paste0(curriculums,collapse=","),")")
      } else if (length(domains) > 0) {
        fcontent <- list(name="Domain", id="DomainId")
        in_select_content <- "cd.id AS `DomainId`, cd.name AS `Domain`"
        in_where <- paste(in_where, "AND cd.id IN (",paste0(domains,collapse=","),")")
      }
      
      in_select_user <- "";
      if (length(users) > 0) {
        fuser <- list(name="User", id="UserId")
        in_select_user <- "uu.id AS `UserId`, CONCAT(uu.name,'<',uu.email,'>') AS `User`"
        in_where <- paste(in_where,"AND uu.id IN (",paste0(users,collapse=","),")")
      } else if (length(classrooms) > 0) {
        fuser <- list(name="Classroom", id="ClassroomId")
        in_select_user <- "uc.id AS `ClassroomId`, CONCAT(us.name,'-',ug.name,'-',uc.name) AS `Classroom`"
        in_where <- paste(in_where,"AND uc.id IN (",paste0(classrooms,collapse=","),")")
      } else if (length(grades) > 0) {
        fuser <- list(name="Grade", id="GradeId")
        in_select_user <- "ug.id AS `GradeId`, CONCAT(us.name,'-',ug.name) AS `Grade`"
        in_where <- paste(in_where,"AND ug.id IN (",paste0(grades,collapse=","),")")
      } else if (length(schools) > 0) {
        fuser <- list(name="School", id="SchoolId")
        in_select_user <- "us.id AS `SchoolId`, us.name AS `School`"
        in_where <- paste(in_where,"AND us.id IN (",paste0(schools,collapse=","),")")
      }
      
      statementSQL <- paste(
        "SELECT", in_select_user, ",", in_select_content, ","
        , "hh.Date_ AS `Date_`,"
        , "hh.Time_ AS `Time_`"
        , "FROM resource_topic rt
        INNER JOIN (",simple_resource_SQL,") cr ON cr.id = rt.resources_id 
        INNER JOIN (
        SELECT ph.problem_id AS resource_id,
        ph.user_id AS user_id,"
        , ifelse(option$typeDate == 'mensal'
                 , "UNIX_TIMESTAMP(DATE_FORMAT(ph.interactionTime,'%Y-%m-01')) AS `Date_`,"
                 , "UNIX_TIMESTAMP(DATE_FORMAT(ph.interactionTime,'%Y-%m-%d')) AS `Date_`,")
        , "          ph.responseTime AS `Time_`
        FROM problemsolvinghistoric ph
        UNION
        SELECT ch.content_id AS resource_id,
        ch.user_id AS user_id,"
        , ifelse(option$typeDate == 'mensal'
                 , "UNIX_TIMESTAMP(DATE_FORMAT(ch.interactionTime,'%Y-%m-01')) AS `Date_`,"
                 , "UNIX_TIMESTAMP(DATE_FORMAT(ch.interactionTime,'%Y-%m-%d')) AS `Date_`,")
        , "          ch.viewTime AS `Time_`
        FROM contentviewhistoric ch
        ) hh ON hh.resource_id = cr.id
        INNER JOIN user_ uu ON uu.id = hh.user_id"
        , "INNER JOIN studentenrollment se ON se.student_id = uu.id
        INNER JOIN classroom uc ON uc.id = se.classroom_id
        INNER JOIN grade ug ON ug.id = uc.grade_id
        INNER JOIN school us ON us.id = ug.school_id"
        , "INNER JOIN topic ct ON ct.id = rt.topics_id
        INNER JOIN curriculum cc ON cc.id = ct.curriculum_id
        INNER JOIN domain_ cd ON cd.id = cc.domain_id"
        , "WHERE", in_where, "AND (hh.Date_ BETWEEN", startDate, "AND", endDate, ")"
        , "ORDER BY", paste0(c(fuser$id,fcontent$id,"Date_"), collapse=","), "ASC"
        )
      
      df <- get_df_from_SQL_Query(statementSQL, c(fuser$name, fcontent$name))
      display_col <- c(fuser$name,fcontent$name,"Date","Time")
      if (nrow(df) > 0) {
        df[["Category"]] <- paste0(df[[fuser$name]],':',df[[fcontent$name]])
        
        gdf <- group_by(df, uid = df[[fuser$id]], uname = df[[fuser$name]]
                        , cid = df[[fcontent$id]], cname = df[[fcontent$name]]
                        , Category = df[['Category']], Date = df[['Date_']])
        if (option$formulaEngagement == "tm") {
          gdf <- as.data.frame(summarise(gdf, Time = mean(Time_)))
        } else {
          gdf <- as.data.frame(summarise(gdf, Time = sum(Time_)))
        }
        
        colnames(gdf)[colnames(gdf)=="uid"] <- fuser$id
        colnames(gdf)[colnames(gdf)=="uname"] <- fuser$name
        colnames(gdf)[colnames(gdf)=="cid"] <- fcontent$id
        colnames(gdf)[colnames(gdf)=="cname"] <- fcontent$name
        df <- gdf
        display_col <- c(fuser$name,fcontent$name,"Category","Date","Time")
      }
      return(list(fcontent=fcontent, fuser=fuser, df=df, SQL=statementSQL, display_col=display_col))
    }
  } else if ("performance-learning:temporal-series" == typeData) {
    
    in_from <- ""
    
    in_select_user <- ""; in_where <- "1=1";
    fuser <- list(); fcontent<-list()
    
    if (option$typePerformance == "pmc") {
      
      in_select_content <- ""
      if (length(curriculums) > 0) {
        fcontent <- list(name="Curriculum", id="CurriculumId")
        in_select_content <- "cc.id AS `CurriculumId`, cc.name AS `Curriculum`"
        in_from <- "curriculumlearninggoalhistoric gh"
        in_from <- paste(in_from, "INNER JOIN curriculum cc ON cc.id = gh.curriculum_id")
        in_where <- paste(in_where, "AND cc.id IN (",paste0(curriculums,collapse=","),")")
      } else if (length(domains) > 0) {
        fcontent <- list(name="Domain", id="DomainId")
        in_select_content <- "cd.id AS `DomainId`, cd.name AS `Domain`"
        in_from <- "domainlearninggoalhistoric gh"
        in_from <- paste(in_from, "INNER JOIN domain_ cd ON cd.id = gh.domain_id")
        in_where <- paste(in_where, "AND cd.id IN (",paste0(domains,collapse=","),")")
      }
      
      
      in_select_user <- "";
      if (length(users) > 0) {
        fuser <- list(name="User", id="UserId")
        in_select_user <- "uu.id AS `UserId`, CONCAT(uu.name,'<',uu.email,'>') AS `User`"
        in_where <- paste(in_where,"AND uu.id IN (",paste0(users,collapse=","),")")
      } else if (length(classrooms) > 0) {
        fuser <- list(name="Classroom", id="ClassroomId")
        in_select_user <- "uc.id AS `ClassroomId`, CONCAT(us.name,'-',ug.name,'-',uc.name) AS `Classroom`"
        in_where <- paste(in_where,"AND uc.id IN (",paste0(classrooms,collapse=","),")")
      } else if (length(grades) > 0) {
        fuser <- list(name="Grade", id="GradeId")
        in_select_user <- "ug.id AS `GradeId`, CONCAT(us.name,'-',ug.name) AS `Grade`"
        in_where <- paste(in_where,"AND ug.id IN (",paste0(grades,collapse=","),")")
      } else if (length(schools) > 0) {
        fuser <- list(name="School", id="SchoolId")
        in_select_user <- "us.id AS `SchoolId`, us.name AS `School`"
        in_where <- paste(in_where,"AND us.id IN (",paste0(schools,collapse=","),")")
      }
      
      statementSQL <- paste(
        "SELECT", in_select_user, ",", in_select_content, ","
        , ifelse(option$typeDate == 'mensal'
                 , "UNIX_TIMESTAMP(DATE_FORMAT(gh.interactionTime,'%Y-%m-01')) AS `Date_`,"
                 , "UNIX_TIMESTAMP(DATE_FORMAT(gh.interactionTime,'%Y-%m-%d')) AS `Date_`,")
        , "gh.value_ AS `Pct`"
        , "FROM", in_from
        , "INNER JOIN user_ uu ON uu.id = gh.user_id
        INNER JOIN studentenrollment se ON se.student_id = uu.id
        INNER JOIN classroom uc ON uc.id = se.classroom_id
        INNER JOIN grade ug ON ug.id = uc.grade_id
        INNER JOIN school us ON us.id = ug.school_id"
        , "WHERE", in_where
        , "AND (gh.interactionTime BETWEEN", paste0("from_unixtime(",startDate,")")
        , "AND", paste0("from_unixtime(",endDate,")"),")"
        , "ORDER BY", paste0(c(fuser$id,fcontent$id,"Date_"), collapse=","), "ASC"
      )
      
      df <- get_df_from_SQL_Query(statementSQL, c(fuser$name, fcontent$name))
      display_col <- c(fuser$name,fcontent$name,"Date")
      if (nrow(df) > 0) {
        df[["Category"]] <- paste0(df[[fuser$name]],':',df[[fcontent$name]])
        
        gdf <- group_by(df, uid = df[[fuser$id]], uname = df[[fuser$name]]
                        , cid = df[[fcontent$id]], cname = df[[fcontent$name]]
                        , Category = df[['Category']], Date = df[['Date_']])
        gdf <- as.data.frame(summarise(gdf, Pct = mean(Pct)))
        
        colnames(gdf)[colnames(gdf)=="uid"] <- fuser$id
        colnames(gdf)[colnames(gdf)=="uname"] <- fuser$name
        colnames(gdf)[colnames(gdf)=="cid"] <- fcontent$id
        colnames(gdf)[colnames(gdf)=="cname"] <- fcontent$name
        df <- gdf
        display_col <- c(fuser$name,fcontent$name,"Category","Date","Pct")
      }
      
      return(list(fcontent=fcontent, fuser=fuser, df=df, SQL=statementSQL, display_col=display_col))
      
    } else {
      
      in_where <- "1=1"; fcontent <- list(); fuser <- list();
      
      in_select_content <- ""
      if (length(resources) > 0) {
        fcontent <- list(name="Resource", id="ResourceId")
        in_select_content <- "cr.id AS `ResourceId`, cr.name AS `Resource`"
        in_where <- paste(in_where, "AND cr.id IN (",paste0(resources,collapse=","),")")
      } else if (length(topics) > 0) {
        fcontent <- list(name="Topic", id="TopicId")
        in_select_content <- "ct.id AS `TopicId`, ct.name AS `Topic`"
        in_where <- paste(in_where, "AND ct.id IN (",paste0(topics,collapse=","),")")
      } else if (length(curriculums) > 0) {
        fcontent <- list(name="Curriculum", id="CurriculumId")
        in_select_content <- "cc.id AS `CurriculumId`, cc.name AS `Curriculum`"
        in_where <- paste(in_where, "AND cc.id IN (",paste0(curriculums,collapse=","),")")
      } else if (length(domains) > 0) {
        fcontent <- list(name="Domain", id="DomainId")
        in_select_content <- "cd.id AS `DomainId`, cd.name AS `Domain`"
        in_where <- paste(in_where, "AND cd.id IN (",paste0(domains,collapse=","),")")
      }
      
      in_select_user <- "";
      if (length(users) > 0) {
        fuser <- list(name="User", id="UserId")
        in_select_user <- "uu.id AS `UserId`, CONCAT(uu.name,'<',uu.email,'>') AS `User`"
        in_where <- paste(in_where,"AND uu.id IN (",paste0(users,collapse=","),")")
      } else if (length(classrooms) > 0) {
        fuser <- list(name="Classroom", id="ClassroomId")
        in_select_user <- "uc.id AS `ClassroomId`, CONCAT(us.name,'-',ug.name,'-',uc.name) AS `Classroom`"
        in_where <- paste(in_where,"AND uc.id IN (",paste0(classrooms,collapse=","),")")
      } else if (length(grades) > 0) {
        fuser <- list(name="Grade", id="GradeId")
        in_select_user <- "ug.id AS `GradeId`, CONCAT(us.name,'-',ug.name) AS `Grade`"
        in_where <- paste(in_where,"AND ug.id IN (",paste0(grades,collapse=","),")")
      } else if (length(schools) > 0) {
        fuser <- list(name="School", id="SchoolId")
        in_select_user <- "us.id AS `SchoolId`, us.name AS `School`"
        in_where <- paste(in_where,"AND us.id IN (",paste0(schools,collapse=","),")")
      }
      
      statementSQL <- paste(
        "SELECT", in_select_user, ",", in_select_content, ","
        , ifelse(option$typeDate == 'mensal'
                 , "UNIX_TIMESTAMP(DATE_FORMAT(ph.interactionTime,'%Y-%m-01')) AS `Date_`,"
                 , "UNIX_TIMESTAMP(DATE_FORMAT(ph.interactionTime,'%Y-%m-%d')) AS `Date_`,")
        , "CASE WHEN ph.correctly_done=(1) THEN 1 ELSE 0 END AS `Done_`"
        , "FROM problemsolvinghistoric ph"
        , "INNER JOIN user_ uu ON uu.id = ph.user_id
           INNER JOIN studentenrollment se ON se.student_id = uu.id
           INNER JOIN classroom uc ON uc.id = se.classroom_id
           INNER JOIN grade ug ON ug.id = uc.grade_id
           INNER JOIN school us ON us.id = ug.school_id"
        , "INNER JOIN (", simple_resource_SQL, ") cr ON cr.id = ph.problem_id
           INNER JOIN resource_topic rt ON rt.resources_id = cr.id
           INNER JOIN topic ct ON ct.id = rt.topics_id
           INNER JOIN curriculum cc ON cc.id = ct.curriculum_id
           INNER JOIN domain_ cd ON cd.id = cc.domain_id"
        , "WHERE", in_where
        , "AND (ph.interactionTime BETWEEN", paste0("from_unixtime(",startDate,")")
        , "AND", paste0("from_unixtime(",endDate,")"),")"
        , "ORDER BY", paste0(c(fuser$id,fcontent$id,"Date_"), collapse=","), "ASC"
      )
      
      df <- get_df_from_SQL_Query(statementSQL, c(fuser$name, fcontent$name))
      display_col <- c(fuser$name,fcontent$name,"Date")
      if (nrow(df) > 0) {
        df[["Category"]] <- paste0(df[[fuser$name]],':',df[[fcontent$name]])
        
        gdf <- group_by(df, uid = df[[fuser$id]], uname = df[[fuser$name]]
                        , cid = df[[fcontent$id]], cname = df[[fcontent$name]]
                        , Category = df[['Category']], Date = df[['Date_']])
        gdf <- as.data.frame(summarise(gdf, sum = sum(Done_), n = n()))
        gdf[["Pct"]] <- gdf[["sum"]]/gdf[["n"]]
        if (option$typePerformance == "pme") {
          gdf[["Pct"]] <- rep(1, nrow(gdf)) - gdf[["Pct"]]
        }
        gdf[["Pct"]] <- gdf[["Pct"]]*100
        
        colnames(gdf)[colnames(gdf)=="uid"] <- fuser$id
        colnames(gdf)[colnames(gdf)=="uname"] <- fuser$name
        colnames(gdf)[colnames(gdf)=="cid"] <- fcontent$id
        colnames(gdf)[colnames(gdf)=="cname"] <- fcontent$name
        df <- gdf
        display_col <- c(fuser$name,fcontent$name,"Category","Date","Pct")
      }
      
      return(list(fcontent=fcontent, fuser=fuser, df=df, SQL=statementSQL, display_col=display_col))
    }
    
  }
  
}

