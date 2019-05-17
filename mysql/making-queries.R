
# returns a pair-list <name,id> for a key
get_choices <- function(key, filters = list(), sql.params = default.sql.params()) {
  if (any(c('school','grade','classroom','user') == key)) {
    statementSQL <- get_userSegmentSQL(key, filters)
  } else if (any(c('domain','curriculum','topic','resource') == key)) {
    statementSQL <- get_contentSegmentSQL(key, filters)
  }
  df <- sql.as.df(statementSQL, c(sql.params[[key]]$name))
  
  to_return <- as.list(df[[sql.params[[key]]$id]])
  names(to_return) <- df[[sql.params[[key]]$name]]
  return(to_return)
}

# get query parameters to MySQL query
get_query.params <- function(dtype, filters, options, sql.params = default.sql.params()) {
  
  to_filters <- list()
  
  if (('learning-performance'!=dtype) & (length(filters[['resource']]) > 0)) {
    fcontent <- 'resource'
    to_filters[['resource']] <- filters[['resource']]
  } else if (length(filters[['topic']]) > 0) {
    fcontent <- 'topic'
    to_filters[['topic']] <- filters[['topic']]
  } else if (length(filters[['curriculum']]) > 0) {
    fcontent <- 'curriculum'
    to_filters[['curriculum']] <- filters[['curriculum']]
  } else if (length(filters[['domain']]) > 0) {
    fcontent <- 'domain'
    to_filters[['domain']] <- filters[['domain']]
  }
  
  if (length(filters[['user']]) > 0) {
    fuser <- "user"
    to_filters[['user']] <- filters[['user']]
  } else if (length(filters[['classroom']]) > 0) {
    fuser <- "classroom"
    to_filters[['classroom']] <- filters[['classroom']]
  } else if (length(filters[['grade']]) > 0) {
    fuser <- "grade"
    to_filters[['grade']] <- filters[['grade']]
  } else if (length(filters[['school']]) > 0) {
    fuser <- "school"
    to_filters[['school']] <- filters[['school']]
  }
  
  return(list(fuser = fuser, fcontent = fcontent, filters = to_filters))
}

# function to get query and making data from MySQL DB
get_data <- function(dtype, filters = list(), options = list(), sql.params = default.sql.params()) {
  statementSQL <- "Click update button to obtain data from DB ..."

  query.params <- get_query.params(dtype, filters, options)
  fuser <- query.params$fuser; fcontent <- query.params$fcontent;
  
  uid <- sql.params[[fuser]]$id; cid <- sql.params[[fcontent]]$id;
  uname <- sql.params[[fuser]]$name; cname <- sql.params[[fcontent]]$name;
  
  df <- data.frame()
  display_col <- c()
  
  if ((dtype == 'learning-performance') | (dtype == 'learning-performance:temporal-series')) {
    
    tserie <- F; startDate = 0; endDate <- 0;
    if (dtype == 'learning-performance:temporal-series') {
      tserie <- T
      endDate <- filters[["endDate"]]
      startDate <- filters[["startDate"]]
    }
    
    if (tserie) {
      if (options$typeLearningPerformanceTemporalSerie == 'pmc') {
        statementSQL <- get_completenessSQL(fuser, fcontent, query.params$filters
                                            , tserie = tserie, tdate = options$typeDate
                                            , startDate = startDate, endDate = endDate)
      } else {
        statementSQL <- get_hitsProblemSQL(fuser, fcontent, query.params$filters
                                           , tserie = tserie, tdate = options$typeDate
                                           , startDate = startDate, endDate = endDate)
      }
    } else {
      if (options$typeLearningPerformance == 'pmc') {
        statementSQL <- get_completenessSQL(fuser, fcontent, query.params$filters, tserie = F)
      } else {
        statementSQL <- get_hitsProblemSQL(fuser, fcontent, query.params$filters, tserie = F)
      }
    }
    
    df <- sql.as.df(statementSQL, encoding_fields = c(uname, cname))
    if (nrow(df) > 0) {
      if (tserie) {
        df[["Category"]] <- paste0(df[[uname]],':',df[[cname]])
        gdf <- group_by(df, uid = df[[uid]], uname = df[[uname]]
                        , cid = df[[cid]], cname = df[[cname]]
                        , Category = df[['Category']], Date = df[['Date_']])
        if (options$typeLearningPerformanceTemporalSerie == 'pmc') {
          gdf <- as.data.frame(summarise(gdf, Pct = mean(Pct)))
        } else {
          gdf <- as.data.frame(summarise(gdf, sum = sum(Done_), n = n()))
          gdf[["Pct"]] <- gdf[["sum"]]/gdf[["n"]]
          if (options$typeLearningPerformanceTemporalSerie == "pme") {
            gdf[["Pct"]] <- rep(1, nrow(gdf)) - gdf[["Pct"]]
          }
          gdf[["Pct"]] <- gdf[["Pct"]]*100
        }
      } else {
        gdf <- group_by(df, uid = df[[uid]], uname = df[[uname]]
                        , cid = df[[cid]], cname = df[[cname]])
        if (options$typeLearningPerformance == 'pmc') {
          gdf <- as.data.frame(summarise(gdf, Pct = mean(Pct)))
        } else {
          gdf <- as.data.frame(summarise(gdf, sum = sum(Done_), n = n()))
          gdf[["Pct"]] <- gdf[["sum"]]/gdf[["n"]]
          if (options$typeLearningPerformance == "pme") {
            gdf[["Pct"]] <- rep(1, nrow(gdf)) - gdf[["Pct"]]
          }
          gdf[["Pct"]] <- gdf[["Pct"]]*100
        }
      }
      df <- gdf[order(gdf[['Pct']]),]
      colnames(df)[colnames(df)=="uid"] <- uid
      colnames(df)[colnames(df)=="uname"] <- uname
      colnames(df)[colnames(df)=="cid"] <- cid
      colnames(df)[colnames(df)=="cname"] <- cname
      display_col <- c(uname,cname,"Pct")
      if (tserie) display_col <- c(uname,cname,"Date","Pct")
    }
  } else { ## engagements
    endDate <- filters[["endDate"]]
    startDate <- filters[["startDate"]]
    statementSQL <- get_engagementSQL(fuser, fcontent, query.params$filters
                                        , tserie = T, tdate = options$typeDate
                                        , startDate = startDate, endDate = endDate)
    
    df <- sql.as.df(statementSQL, encoding_fields = c(uname, cname))
    if (nrow(df) > 0) {
      df[["Category"]] <- paste0(df[[uname]],':',df[[cname]])
      gdf <- group_by(df, uid = df[[uid]], uname = df[[uname]]
                      , cid = df[[cid]], cname = df[[cname]]
                      , Category = df[['Category']], Date = df[['Date_']])
      
      if (options$formulaLearningEngagementTemporalSerie == "tm") {
        gdf <- as.data.frame(summarise(gdf, Time = mean(Time_)))
      } else {
        gdf <- as.data.frame(summarise(gdf, Time = sum(Time_)))
      }
      df <- gdf[order(gdf[['Category']]),]
      colnames(df)[colnames(df)=="uid"] <- uid
      colnames(df)[colnames(df)=="uname"] <- uname
      colnames(df)[colnames(df)=="cid"] <- cid
      colnames(df)[colnames(df)=="cname"] <- cname
      
      display_col <- c(uname,cname,"Category","Date","Time")
    }
    
  }
  
  return(list(fuser=list(id=uid,name=uname), fcontent=list(id=cid,name=cname)
              , df=df, SQL=statementSQL, display_col=display_col))
}
