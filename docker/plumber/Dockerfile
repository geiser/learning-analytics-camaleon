FROM trestletech/plumber
LABEL maintainer="geiser@usp.br"
MAINTAINER geiser <geiser@usp.br>

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libmariadbclient-dev
RUN apt-get clean && apt-get autoclean && apt-get autoremove

# Install R packages
RUN install2.r DBI
RUN install2.r RMySQL
RUN install2.r config

RUN install2.r plotly
RUN install2.r rjson
RUN install2.r qdap

# Making executable
EXPOSE 8080
ENTRYPOINT ["R", "-e", "setwd('/app'); pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8080, swagger=T)"]
CMD ["/app/api-plumber.R"]
