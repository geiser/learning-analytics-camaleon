# api-plumber.R
source('mysql/common.R')
source('mysql/statements.R')
source('mysql/making-queries.R')

#* Return a JSON object of choices that can be used to make the segmentation of content and users. e.g. {name-school1: id1, name-school2: id2, ...}. Such object is very userful to generate <option> elements in HTML input elements. E.g. curl --data '{"key":"user", "filters":{"school": [171]}}' -X POST "http://127.0.0.1:8000/learning-analytics/v0.09/choices" -H  "accept: application/json"
#* @param key The type of choices to be returned. For the users segmentation, the possible values are ('school','grade','classroom','user'), whereas the posible values for the content segmentation are ('domain','curriculum','topic','resource').
#* @param filters The filters in JSON format to be applied in the choices. Thus, for example, when the filter {"school": [id1, id2, id3]} is applied in the choices of users (key='user'), the choices to be returned consist in a pair-list of name and identifiers of users that are part of the schools [id1, id2 and id3].
#* @post /learning-analytics/v0.09/choices
#* @get /learning-analytics/v0.09/choices
#* @serializer unboxedJSON
function(key, filters = list()) {
  get_choices(key, filters)  
}


#* Return a JSON object with the learning-analytic data. E.g. curl --data '{"filters":{"grade": [1344,1345], "domain": [43, 44, 45]}, "options":{"typeLearningPerformance": "pmc"}}' -X POST "http://127.0.0.1:8000/learning-analytics/v0.09/learning-performance" -H  "accept: application/json"
#* @param dtype:character The data type to be obtained from the learning analytics API. The current possible types are 'learning-performance', 'learning-performance-temporal-series', and 'learning-engagement-temporal-series'.
#* @param filters The filters in JSON format to be applied in the selection of data. The current available filters are {'school','grade','classroom','user'} for user segmentation, {'domain','curriculum','topic','resource'} for content segmentation, and {'startDate' and 'endDate'} for temporal segmentation using unix-timestamp format.  Thus, for example, when the filter is {"school": [sid1, sid2, sid3], "topic": [tid1, tid2, tid3]}, the learning analytics corresponds to the schools with identifiers {sid1, sid2, and sid3} and topics with identifiers {tid1, tid2, and tid3}.
#* @param options The options in JSON format to be applied in the learning analytic. The current available options are: (1) {"typeDate": 'daily' or 'monthly'} to indicate if the data in time series will be obtained as monthly or daily time series; (2) {"typeLearningPerformance": 'pmc', 'pma' or 'pme'} to indicate the type of learning performance in the learning analytic of 'learning-performance' and 'learning-performance-temporal-series', where 'pcm' is the avg completeness pct (Percentual Médio de Completude), 'pma' is the avg hit pct (Percentual Médio de Acerto), and 'pme' is the avg error pct (Percentual Médio de Erro); (3) {"typeLearningEngagement": 'te', 'ti' or 'to'} to indicate the type of learning engagement in the learning analytic of 'learning-engagement-temporal-series', where 'te' is the effective time (Tempo efetivo), 'ti' is the interaction time (Tempo de interaçao), and 'to' is observation time (Tempo de observação); and (4) {"formulaLearningEngagement": 'tm' or 'ta'} to indicate the calculate way of engagement in the learning analytic of 'learning-engagement-temporal-series', where 'tm' is the mean time (Tempo medio), and 'ta' is the accumulated time (Tempo acumulado).
#* @param full.info:bool The JSON object to be returned contains metadas "fuser": user segmentation, "fcontent": content segmentation, and "SQL": Query SQL to gather the data. The key "df": containts the data when full.info is TRUE
#* @post /learning-analytics/v0.09/<dtype>
#* @get /learning-analytics/v0.09/<dtype>
#* @serializer unboxedJSON
get_rest_data <- function(dtype, filters = list(), options = list(), full.info = F) {
  
  if (!is.null(options[["typeLearningPerformance"]])) {
    options[["typeLearningPerformanceTemporalSerie"]] <- options[["typeLearningPerformance"]]
  }
  if (!is.null(options[["typeLearningEngagement"]])) {
    options[["typeLearningEngagementTemporalSerie"]] <- options[["typeLearningEngagement"]]
  }
  if (!is.null(options[["typeLearningEngagement"]])) {
    options[["formulaLearningEngagementTemporalSerie"]] <- options[["formulaLearningEngagement"]]
  }
  
  sd <- get_data(dtype, filters = filters, options = options)
  df <- sd$df
  if (any(colnames(df) %in% c('Date')) & nrow(df) > 0) {
    df <- df[order(df$Date),]  
    df$Date <- as.POSIXct(df$Date, origin='1970-01-01', tz ="America/Maceio")
  }
  sd$df <- df
  if (full.info) return(sd) else return(sd$df)
}


#* Return a JSON object according to the specification "https://help.plot.ly/json-chart-schema/" for the learning-analytic data. E.g. curl --data '{"filters":{"grade": [1344,1345], "domain": [43, 44, 45]}, "options":{"typeLearningPerformance": "pmc"}}' -X POST "http://127.0.0.1:8000/learning-analytics/v0.09/learning-performance/as.plotly/radar" -H "accept: application/json"
#* @param dtype:character The data type to be obtained from the learning analytic. The current possible types are 'learning-performance', 'learning-performance-temporal-series', and 'learning-engagement-temporal-series'.
#* @param ctype:character The chart type to be obtained from the learning analytic. The current possible types are 'radar' (radar chart), 'dot' (dot chart), and 'tserie' (time-serie chart).
#* @param filters The filters in JSON format to be applied in the selection of data. The current available filters are {'school','grade','classroom','user'} for user segmentation, {'domain','curriculum','topic','resource'} for content segmentation, and {'startDate' and 'endDate'} for temporal segmentation using unix-timestamp format.  Thus, for example, when the filter is {"school": [sid1, sid2, sid3], "topic": [tid1, tid2, tid3]}, the learning analytics corresponds to the schools with identifiers {sid1, sid2, and sid3} and topics with identifiers {tid1, tid2, and tid3}.
#* @param options The options in JSON format to be applied in the learning analytic. The current available options are: (1) {"typeDate": 'daily' or 'monthly'} to indicate if the data in time series will be obtained as monthly or daily time series; (2) {"typeLearningPerformance": 'pmc', 'pma' or 'pme'} to indicate the type of learning performance in the learning analytic of 'learning-performance' and 'learning-performance-temporal-series', where 'pcm' is the avg completeness pct (Percentual Médio de Completude), 'pma' is the avg hit pct (Percentual Médio de Acerto), and 'pme' is the avg error pct (Percentual Médio de Erro); (3) {"typeLearningEngagement": 'te', 'ti' or 'to'} to indicate the type of learning engagement in the learning analytic of 'learning-engagement-temporal-series', where 'te' is the effective time (Tempo efetivo), 'ti' is the interaction time (Tempo de interaçao), and 'to' is observation time (Tempo de observação); and (4) {"formulaLearningEngagement": 'tm' or 'ta'} to indicate the calculate way of engagement in the learning analytic of 'learning-engagement-temporal-series', where 'tm' is the mean time (Tempo medio), and 'ta' is the accumulated time (Tempo acumulado).
#* @post /learning-analytics/v0.09/<dtype>/as.plotly/<ctype>
#* @get /learning-analytics/v0.09/<dtype>/as.plotly/<ctype>
#* @serializer unboxedJSON
function(dtype, ctype, filters = list(), options = list()) {
  library(plotly)
  # RadarChart, Dot Chart
  sd <- get_rest_data(dtype, filters = filters, options = options, full.info = T)
  to_return <- list("ERROR"="The chart type for this learning analytic is not available")
  
  if (dtype == 'learning-performance') {
    
    title_ <- ""
    if (options[["typeLearningPerformance"]] == "pmc") {
      title_ <- "Percentual Médio de Completude"
    } else if (options[["typeLearningPerformance"]] == "pma") {
      title_ <- "Percentual Médio de Acerto"
    } else if (options[["typeLearningPerformance"]]== "pme") {
      title_ <- "Percentual Médio de Erro"
    }
    
    if (ctype == 'radar') {
      p <- plot_ly(type = 'scatterpolar', fill = 'toself')
      for (u_id in unique(sd$df[[sd$fuser$id]])) {
        u_idx <- (sd$df[[sd$fuser$id]] == u_id)
        p <- add_trace(
          p
          , r = sd$df$Pct[u_idx]
          , theta = sd$df[[sd$fcontent$name]][u_idx]
          , name = head(sd$df[[sd$fuser$name]][u_idx], 1)
        )
      }
      p <- layout(p, legend=list(orientation = 'h'), title=title_)
      to_return <- rjson::fromJSON(plotly_json(p, FALSE))
    } else if (ctype == 'dot') {
      p <- plot_ly(type = 'scatter', mode = "markers")
      for (u_id in unique(sd$df[[sd$fuser$id]])) {
        u_idx <- (sd$df[[sd$fuser$id]] == u_id)
        x <- sd$df$Pct[u_idx]
        y <- sd$df[[sd$fcontent$name]][u_idx]
        p <- add_trace(p, x = x, y = y
                       , name = head(sd$df[[sd$fuser$name]][u_idx],1)
                       , type = 'scatter', mode = "markers"
        )
      }
      p <- layout(p, legend=list(orientation = 'h'), title=title_)
      to_return <- rjson::fromJSON(plotly_json(p, FALSE))
    }
    
  } else if (dtype == 'learning-performance-temporal-series') {
    
    title_ <- "Série Temporal"
    if (options[["formulaLearningEngagement"]] == "tm") {
      title_ <- paste(title_,":","Tempo Médio")
    } else if (options[["formulaLearningEngagement"]] == "ta") {
      title_ <- paste(title_,":","Tempo Acumulado")
    }
    if (options[["formulaLearningEngagement"]] != "te") {
      title_ <- paste("ERROR: Only 'tempo efetivo' AVAILABLE")
    } else {
      title_ <- paste(title_, "Efetivo")
    }
    
    if (ctype == 'tserie') {
      p <- plot_ly(type='scatter', mode='lines+markers')
      for (cat_name in unique(sd$df$Category)) {
        idx <- (sd$df$Category == cat_name);
        p <- add_trace(
          p
          , x = sd$df$Date[idx]
          , y = sd$df$Time[idx]
          , name = cat_name)
      }
      p <- layout(p, legend=list(orientation = 'h'), title=title_)
      to_return <- rjson::fromJSON(plotly_json(p, FALSE))
    }
    
  } else if (dtype == 'learning-engagement-temporal-series') {
    title_ <- "Série Temporal"
    if (input$formulaLearningEngagementTemporalSerie == "tm") {
      title_ <- paste(title_,":","Tempo Médio")
    } else if (input$formulaLearningEngagementTemporalSerie == "ta") {
      title_ <- paste(title_,":","Tempo Acumulado")
    }
    if (input$typeLearningEngagementTemporalSerie != "te") {
      title_ <- paste("ERROR: Only tempo efetivo AVAILABLE")
    } else {
      title_ <- paste(title_, "Efetivo")
    }
    
    if (ctype == 'tserie') {
      p <- plot_ly(type='scatter', mode='lines+markers')
      for (cat_name in unique(sd$df$Category)) {
        idx <- (sd$df$Category == cat_name);
        p <- add_trace(
          p
          , x = sd$df$Date[idx]
          , y = sd$df$Time[idx]
          , name = cat_name
        )
      } 
      p <- layout(p, legend=list(orientation = 'h'), title=title_)
      to_return <- rjson::fromJSON(plotly_json(p, FALSE))
    }
  }
  
  return(to_return)
}
