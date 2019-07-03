FROM rocker/shiny
LABEL maintainer="geiser@usp.br"
MAINTAINER geiser <geiser@usp.br>

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
	libxml2-dev \
	libssl-dev \
    libmariadbclient-dev

RUN apt-get clean && apt-get autoclean && apt-get autoremove

# Install R packages
RUN install2.r DBI
RUN install2.r RMySQL
RUN install2.r config

RUN install2.r httr
RUN install2.r plotly
RUN install2.r qdap

RUN install2.r shinythemes
