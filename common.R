source('mysql/common.R')
source('mysql/statements.R')
source('mysql/making-queries.R')

# get default parameters for working with time
default.time.params <- function() {
  return(list(
    origin='1970-01-01'
    , format = "%Y-%m-%d"
    , format.by.month = "%Y-%m-01"
    , tz ="UTC"
  ))
}

# convert temporal serie data to data frame with zero values for filling data that is not find
tserie.as.filled.df <- function(pdates, values, startDate, endDate, typeDate = 'daily', tparams = default.time.params()) {
  
  startDate <- as.Date(as.POSIXct(as.numeric(startDate), origin = tparams$origin, tz = tparams$tz))
  endDate <- as.Date(as.POSIXct(as.numeric(endDate), origin = tparams$origin, tz = tparams$tz))
  tdates <- seq(from=startDate, to=endDate, by='day')
  
  if (typeDate == "monthly") {
    tdates <- seq(from=as.Date(format(startDate, tparams$format.by.month))
                  , to=as.Date(format(endDate, tparams$format.by.month)), by='month')
  }
  
  x <- c(pdates, setdiff(tdates, pdates))
  y <- c(values, rep(0, length(x)-length(values)))
  y[is.na(y)] <- 0
  y[is.nan(y)] <- 0
  y[is.null(y)] <- 0
  y[length(y) == 0] <- 0
  y[is.character(y) && nchar(trim.space(y)) == 0] <- 0
  
  df <- data.frame(x=x, y=y)
  return(df[order(df$x),])
}

# get data as data frame
get_data.as.df <- function(dtype, filters = list(), options = list()) {
  sd <- get_data(dtype, filters = filters, options = options)
  df <- sd$df
  if (any(colnames(df) %in% c('Date')) & nrow(df) > 0) {
    tparams <- default.time.params()
    df <- df[order(df$Date),]
    df$Date <- as.Date(as.POSIXct(as.numeric(df$Date), origin=tparams$origin, tz=tparams$tz))
    if (options$typeDate == 'monthly') {
      df$Date <- as.Date(format(df$Date, tparams$format.by.month))
    }
  }
  return(df[sd$display_col])
}

# get plotly object for a query data
get_plotly <- function(dtype, ctype, filters = list(), options = list()) {
  library(plotly)
  
  sd <- get_data(dtype, filters = filters, options = options)
  if (any(colnames(sd$df) %in% c('Date')) & nrow(sd$df) > 0) {
    df <- sd$df[order(sd$df$Date),]
    tparams <- default.time.params()
    df$Date <- as.Date(as.POSIXct(as.numeric(df$Date), origin=tparams$origin, tz=tparams$tz))
    if (options$typeDate == 'monthly') {
      df$Date <- as.Date(format(df$Date, tparams$format.by.month))
    }
    sd$df <- df
  }
  

  p <- plot_ly(type='scatter', mode='lines+markers')
  # RadarChart, Dot Chart
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
    }
    
  } else if (dtype == 'learning-performance-temporal-series') {
    
    title_ <- "Série Temporal"
    if (options[["typeLearningPerformance"]] == "pmc") {
      title_ <- paste(title_,":","Percentual Médio de Completude")
    } else if (options[["typeLearningPerformance"]] == "pma") {
      title_ <- paste(title_,":","Percentual Médio de Acerto")
    } else if (options[["typeLearningPerformance"]] == "pme") {
      title_ <- paste(title_,":","Percentual Médio de Erro")
    }
    
    if (ctype == 'tserie') {
      p <- plot_ly(type='scatter', mode='lines+markers')
      for (cat_name in unique(sd$df$Category)) {
        idx <- (sd$df$Category == cat_name);
        ts_df <- tserie.as.filled.df(
          sd$df$Date[idx], sd$df$Pct[idx]
          , filters[["startDate"]], filters[["endDate"]]
          , typeDate = options$typeDate)
        p <- add_trace(
          p
          , x = ts_df$x
          , y = ts_df$y
          , name = cat_name)
      }
      p <- layout(p, legend=list(orientation = 'h'), title=title_)
    }
    
  } else if (dtype == 'learning-engagement-temporal-series') {
    
    title_ <- "Série Temporal"
    if (options[["formulaLearningEngagement"]] == "tm") {
      title_ <- paste(title_,":","Tempo Médio")
    } else if (options[["formulaLearningEngagement"]] == "ta") {
      title_ <- paste(title_,":","Tempo Acumulado")
    }
    
    if (options[["typeLearningEngagement"]] == "te") {
      title_ <- paste(title_,":","Tempo Efetivo")
    } else if (options[["typeLearningEngagement"]] == "ti") {
      title_ <- paste(title_, "Tempo Interacao")
    } else if (options[["typeLearningEngagement"]] == "to") {
      title_ <- paste(title_, "Tempo de Observacao")
    }
    
    if (ctype == 'tserie') {
      p <- plot_ly(type='scatter', mode='lines+markers')
      for (cat_name in unique(sd$df$Category)) {
        idx <- (sd$df$Category == cat_name);
        ts_df <- tserie.as.filled.df(
          sd$df$Date[idx], sd$df$Time[idx]
          , filters[["startDate"]], filters[["endDate"]]
          , typeDate = options$typeDate)
        p <- add_trace(
          p
          , x = ts_df$x
          , y = ts_df$y
          , name = cat_name
        )
      } 
      p <- layout(p, legend=list(orientation = 'h'), title=title_)
    }
  }
  
  return(p)
}
