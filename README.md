# learning-analytics-camaleon
Learning analytics for Camaleon
- Online demo in: http://geiser.tech:3838/learning-analytics-camaleon/
- REST API: http://geiser.tech:8080/
- Swagger documentation for the REST API in: http://geiser.tech:8080/__swagger__/

## Install Procedure

Requirements:
- Docker (>= 18.06.0)
- Docker Compose (>= 1.22.0)

Procedure to install requirements in Ubuntu 18.04:
```
sudo apt-get purge docker docker-engine docker.io docker-ce
sudo apt-get update
sudo apt-get install apt-transport-https ca-certificates curl software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add
sudo add-apt-repository "deb [arch=amd64] https://download.docker.com/linux/ubuntu  $(lsb_release -cs)  stable" 
sudo apt-get update
sudo apt-get install docker-ce
```

Procedure to update docker compose in Ubuntu 18.04
```
sudo curl -L "https://github.com/docker/compose/releases/download/1.23.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose
docker-compose --version
```


Setup the docker as service and run it at startup
```
sudo systemctl start docker
sudo systemctl enable docker
```

1. Download source code:
```
git clone https://github.com/geiser/learning-analytics-camaleon.git
```

2. Configure database in file `config.yml` (example is available in `config.default.yml`)

3. Configure and build imagens to run the project as container
```
cd learning-analytics-camaleon
./configure
make
```
  * Use `make up` to spin up all the services of the project
  * Use `make run service={service}` to run only one service {plumber, shiny} of the project. E.g. `make run service=shiny`
  * Use `make help` to view more options

4. Install the project as service using systemd
```
sudo make install
```

To start a service {plumber, shiny} of the project and to run at startup, You need to use the following codes:
```
sudo systemctl start learning-analytics-camaleon@{service}
sudo systemctl enable learning-analytics-camaleon@{service}
sudo systemctl daemon-reload
```
For the plumber service, use:
```
sudo systemctl start learning-analytics-camaleon@plumber
sudo systemctl start learning-analytics-camaleon@plumber
sudo systemctl daemon-reload
```

## REST API


### get_rest_choices

| Item | Detail |
|--------|----------|
| Method | @get @post |
| URL | /learning-analytics/v0.09/choices |
| Description | Return a JSON object of choices that can be used to make the segmentation of content and users. e.g. {name-school1: id1, name-school2: id2, ...}. Such object is very userful to generate <option> elements in HTML input elements. |

| Param | Type | Description |
|-------|-----|------------|
| key | string | The type of choices to be returned. For the users segmentation, the possible values are: {'school', 'grade', 'classroom', 'user'}, whereas the posible values for the content segmentation are {'domain', 'curriculum', 'topic', 'resource'} |
| filters | JSON | The filters in JSON format to be applied in the choices. Thus, for example, when the filter {"school": [id1, id2, id3]} is applied in the choices of users (key='user'), the choices to be returned consist in a pair-list of name and identifiers of users that are part of the schools [id1, id2, id3] |

Examples:
```
curl --data '{"key":"user", "filters":{"school": [171]}}' -X POST "http://127.0.0.1:8080/learning-analytics/v0.09/choices" -H  "accept: application/json" 
```   


### get_rest_data
  
| Item | Detail |
|--------|----------|
| Method | @get @post |
| URL | /learning-analytics/v0.09/{dtype} |
| Description | Return a JSON object with the learning-analytic data |


| Param | Type | Description |
|-------|-----|------------|
| dtype | string | The data type to be obtained from the learning analytics API. The current possible types are: {'learning-performance', 'learning-performance-temporal-series', 'learning-engagement-temporal-series'} |
| filters | JSON | The filters in JSON format to be applied in the selection of data. The current available filters are {'school', 'grade', 'classroom', 'user'} for user segmentation, {'domain', 'curriculum', 'topic', 'resource'} for content segmentation, and {'startDate'} and {'endDate'} for temporal segmentation using unix-timestamp format.  Thus, for example, when the filter is {"school": [sid1, sid2, sid3], "topic": [tid1, tid2, tid3]}, the learning analytics corresponds to the schools with identifiers "sid1", "sid2", and "sid3", and topics with identifiers "tid1", "tid2", and "tid3". |
| options | JSON | The options in JSON format to be applied in the learning analytic. The current available options are: (1) {"typeDate": 'daily' or 'monthly'} to indicate if the data in time series will be obtained as monthly or daily time series; (2) {"typeLearningPerformance": 'pmc', 'pma' or 'pme'} to indicate the type of learning performance in the learning analytic of 'learning-performance' and 'learning-performance-temporal-series', where 'pcm' is the avg completeness pct (Percentual Médio de Completude), 'pma' is the avg hit pct (Percentual Médio de Acerto), and 'pme' is the avg error pct (Percentual Médio de Erro); (3) {"typeLearningEngagement": 'te', 'ti' or 'to'} to indicate the type of learning engagement in the learning analytic of 'learning-engagement-temporal-series', where 'te' is the effective time (Tempo efetivo), 'ti' is the interaction time (Tempo de interaçao), and 'to' is observation time (Tempo de observação); and (4) {"formulaLearningEngagement": 'tm' or 'ta'} to indicate the calculate way of engagement in the learning analytic of 'learning-engagement-temporal-series', where 'tm' is the mean time (Tempo medio), and 'ta' is the accumulated time (Tempo acumulado) |
| full.info | bool | The JSON object to be returned contains the metadas: "fuser" (user segmentation), "fcontent" (content segmentation), and "SQL" (Query SQL to gather the data). The key "df" containts the data when full.info is TRUE |

Examples:
```
curl --data '{"filters":{"grade": [1344,1345], "domain": [43, 44, 45]}, "options":{"typeLearningPerformance": "pmc"}}' -X POST "http://127.0.0.1:8080/learning-analytics/v0.09/learning-performance" -H  "accept: application/json" 
```


### get_rest_plotly

| Item | Detail |
|--------|----------|
| Method | @get @post |
| URL | /learning-analytics/v0.09/{dtype}/as.plotly/{ctype} |
| Description | Return a JSON plotly object according to the specification "https://help.plot.ly/json-chart-schema/" for the learning-analytic data |


| Param | Type | Description |
|-------|-----|------------|
| dtype | string | The data type to be obtained from the learning analytics API. The current possible types are: {'learning-performance', 'learning-performance-temporal-series', 'learning-engagement-temporal-series'} |
| ctype | string | The chart type to be obtained from the learning analytic. The current possible types are 'radar' (radar chart), 'dot' (dot chart), and 'tserie' (time-serie chart) |
| filters | JSON | The filters in JSON format to be applied in the selection of data. The current available filters are {'school', 'grade', 'classroom', 'user'} for user segmentation, {'domain', 'curriculum', 'topic', 'resource'} for content segmentation, and {'startDate'} and {'endDate'} for temporal segmentation using unix-timestamp format.  Thus, for example, when the filter is {"school": [sid1, sid2, sid3], "topic": [tid1, tid2, tid3]}, the learning analytics corresponds to the schools with identifiers "sid1", "sid2", and "sid3", and topics with identifiers "tid1", "tid2", and "tid3". |
| options | JSON | The options in JSON format to be applied in the learning analytic. The current available options are: (1) {"typeDate": 'daily' or 'monthly'} to indicate if the data in time series will be obtained as monthly or daily time series; (2) {"typeLearningPerformance": 'pmc', 'pma' or 'pme'} to indicate the type of learning performance in the learning analytic of 'learning-performance' and 'learning-performance-temporal-series', where 'pcm' is the avg completeness pct (Percentual Médio de Completude), 'pma' is the avg hit pct (Percentual Médio de Acerto), and 'pme' is the avg error pct (Percentual Médio de Erro); (3) {"typeLearningEngagement": 'te', 'ti' or 'to'} to indicate the type of learning engagement in the learning analytic of 'learning-engagement-temporal-series', where 'te' is the effective time (Tempo efetivo), 'ti' is the interaction time (Tempo de interaçao), and 'to' is observation time (Tempo de observação); and (4) {"formulaLearningEngagement": 'tm' or 'ta'} to indicate the calculate way of engagement in the learning analytic of 'learning-engagement-temporal-series', where 'tm' is the mean time (Tempo medio), and 'ta' is the accumulated time (Tempo acumulado) |
| full.info | bool | The JSON object to be returned contains the metadas: "fuser" (user segmentation), "fcontent" (content segmentation), and "SQL" (Query SQL to gather the data). The key "df" containts the data when full.info is TRUE |

Examples:
```
curl --data '{"filters":{"grade": [1344,1345], "domain": [43, 44, 45]}, "options":{"typeLearningPerformance": "pmc"}}' -X POST "http://127.0.0.1:8080/learning-analytics/v0.09/learning-performance/as.plotly/radar" -H "accept: application/json"
```


>  Copyright Geiser Chalco Challco <geiser@usp.br>
   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at
       http://www.apache.org/licenses/LICENSE-2.0
       
       
