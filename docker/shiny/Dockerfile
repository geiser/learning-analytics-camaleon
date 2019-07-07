FROM geiser/r-shiny:v0.09 AS prod
MAINTAINER Geiser Chalco <geiser@usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="Learning Analytics for Camaleon through R-shiny" \
      maintainer="Geiser Chalco <geiser@usp.br>"

RUN apt-get update -qq && apt-get install -y \
	libmariadbclient-dev \
	&& apt-get clean && apt-get autoclean && apt-get autoremove \
	&& rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r DBI \
	&& install2.r RMySQL

