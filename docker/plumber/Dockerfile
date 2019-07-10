FROM geiser/r-plumber:v0.09 AS prod
MAINTAINER Geiser Chalco <geiser@usp.br>

LABEL org.label-schema.license="GPL-3.0" \
      org.label-schema.vcs-url="https://github.com/geiser/docker" \
      org.label-schema.vendor="Learning Analytics for Camaleon through R-plumber" \
      maintainer="Geiser Chalco <geiser@usp.br>"

RUN apt-get update -qq && apt-get install -y \
	libmariadbclient-dev \
	&& apt-get clean && apt-get autoclean && apt-get autoremove \
	&& rm -rf /var/lib/apt/lists/*

# Install R packages
RUN install2.r -s DBI \
	&& install2.r -s RMySQL

# Making executable
EXPOSE 8080
ENTRYPOINT ["R", "-e", "setwd('/app'); pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8080, swagger=T)"]
CMD ["/app/api-plumber.R"]
