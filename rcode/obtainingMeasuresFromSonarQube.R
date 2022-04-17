# Load packages
library(httr)
library(rlist)
library(jsonlite)
library(listviewer)
library(tidyverse)
library(repurrrsive)


#Due to SonarQube api evolution we should always
#review web api documentation of the SonarQube instance:
#https://sonarqube2.inf.uva.es/web_api/api/

# Base enpoint as variable
url_sonar <- 'https://sonarqube2.inf.uva.es/' # Your Sonar instance
# Authentication with token generation in SonarQube instance
token<- 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxx' # Your security token at the SonarQube instance

sonar_web_api_request_get <- function(command, parameters, pageIndex='') {
  petition <- paste0(url_sonar, command, parameters, pageIndex)
  # Construct API request with authentication
  GET(url = petition, authenticate(user=token,password = '') )
}

# Command
to_obtain_all_metrics <- 'api/metrics/search?'
# Parameters
params <- ''

#Inicialization
i <- 1
metrics_data_frame <- NULL
repeat {
  # Construct API request with authentication and pagination
  pagination <- paste0('p=', i)
  metrics <- sonar_web_api_request_get(to_obtain_all_metrics, params, pagination)
  
  ## Examine response components
  #str(metrics)
  #names(metrics)
  # Process API request content 
  metrics_content <- content(metrics)
  ## Examine response content
  # jsonedit(metrics_content, mode='view')
  
  # Apply function across all elements (each metrics in Sonar)
  # to extract the key, name, description and type
  metrics_data_frame_sub <- lapply(metrics_content$metrics, function(x) {
    df <- data_frame(key          = x$key,
                     name          = x$name,
                     type          = x$type
    )
  }) %>% bind_rows()
  metrics_data_frame <- bind_rows(metrics_data_frame, metrics_data_frame_sub)
  # ps is pagesize in this API
  how_many_pages <- metrics_content$total/metrics_content$ps
  
  if (i < how_many_pages) {i <- i+1}
  else {break}
}

# Uncomment to view data frame
#View(metrics_data_frame)


# Obtaining all project with TDS in name. It is case insensitive.
# Command
obtain_projects <- 'api/projects/index?'
# Parameters
params <- 'search=TDS'

# Construct API request with authentication. There is no pagination in result
pagination <- ''
projects <- sonar_web_api_request_get(obtain_projects, params, pagination)
  
# # Examine response components
# names(projects)
# Process API request content
projects_content <- content(projects)

# Uncomment for json fancy visualization
#jsonedit(projects_content, mode='view')
  
# Keep the projects in our study.
filtered_projects <- list.filter(projects_content, grepl('*-tds-*',k))
  
# Apply function across all elements (each project in SonarQube server in our study) 
# to extract the key, id and name
projects_data_frame <- lapply(filtered_projects, function(x) {
    df <- data_frame(key          = x$k,
                     id           = x$id,
                     name         = x$nm
    )
  }) %>% bind_rows()

# Uncomment to view the data frame  
#View(projects_data_frame) 


# Obtain some metrics to project level of given projects
# iterate over de projectKeys and integrate results
projectKeys <- projects_data_frame$key
# Define which metrics are going to be obtained
metricsKeys <- c('lines', 'ncloc', 'comment_lines', 'classes',
                 'complexity', 
                 'violations','blocker_violations','major_violations','minor_violations','info_violations',
                 'vulnerabilities','bugs','code_smells',
                 'duplicated_blocks', 'duplicated_lines','duplicated_lines_density',
                 'sqale_rating','sqale_index','sqale_debt_ratio', 'effort_to_reach_maintainability_rating_a',
                 'tests','test_success_density',
                 'coverage','line_coverage','branch_coverage','overall_branch_coverage',
                 'conditions_to_cover', 'overall_conditions_to_cover'
)

# Command
to_obtain_measures_of_some_metrics <- 'api/measures/component?'
# Parameters: projectKeys and metricKeys are used as parameters
# Initialization
projects_measures_data_frame <- NULL
for (projectKey in projectKeys) {
    params_list <- list(component = paste0('componentKey=', projectKey), 
                         metricsKeys = paste0('metricKeys=',paste0(metricsKeys, collapse=','))
    )
    params <- paste0(params_list, collapse='&')
    # Construct API request with authentication (no pagination)
    measures <- sonar_web_api_request_get(to_obtain_measures_of_some_metrics,params)

  # Inicialization (measures per project)
  measures_data_frame <- NULL
  ## Examine response components
  #names(measures)
  # Process API request content 
  measures_content <- content(measures)
  ## Examine response content
  #jsonedit(measures_content, mode='view')

  # Apply function across all list elements (each metric in the request)
  # to extract the metric name and measure value
  measures_data_frame <- lapply(measures_content$component$measures, function(x) {
  df <- data_frame(
    metric = x$metric,
    value = x$value
  )
  }) %>% bind_rows() %>% spread(metric, value) 

  measures_data_frame$project <- c(projectKey)

  #change column order in data frame
  column_order <- c("project", metricsKeys)
  project_measures_data_frame <- measures_data_frame[, column_order]
  projects_measures_data_frame <- bind_rows(projects_measures_data_frame, project_measures_data_frame)
}

# Uncomment to view data frame
# View(projects_measures_data_frame)

# Response of the request is treated as character type. Convert the type of measures' values to numeric 
projects_measures_data_frame_asnumbers <- modify_at(projects_measures_data_frame, metricsKeys, as.numeric)

# Creating new columns
projects_measures_data_frame_asnumbers$comment_density <- with(projects_measures_data_frame_asnumbers, comment_lines/(ncloc+comment_lines))
projects_measures_data_frame_asnumbers$smells_density <- with(projects_measures_data_frame_asnumbers, code_smells/ncloc)
projects_measures_data_frame_asnumbers$code_to_test <- with(projects_measures_data_frame_asnumbers, tests/ncloc)

# Some statistics in general
summary(projects_measures_data_frame_asnumbers)

# Separating groups of projects

#2016-2017
#pair (p3)
p3_2016_2017_projects <- dplyr::filter(projects_measures_data_frame_asnumbers,substr(project, 1, 15)=='2016-2017-tds-e')
#single (p4)
p4_2016_2017_projects <- dplyr::filter(projects_measures_data_frame_asnumbers,substr(project, 1, 15)=='2016-2017-tds-a')

#2017-2018
#pair (p3)
p3_2017_2018_projects <- dplyr::filter(projects_measures_data_frame_asnumbers,substr(project, 1, 15)=='2017-2018-tds-e')
#single (p4)
p4_2017_2018_projects <- dplyr::filter(projects_measures_data_frame_asnumbers,substr(project, 1, 15)=='2017-2018-tds-a')

#2018-2019
#pair (p3)
p3_2018_2019_projects <- dplyr::filter(projects_measures_data_frame_asnumbers,substr(project, 1, 15)=='2018-2019-tds-e')
#single (p4)
p4_2018_2019_projects <- dplyr::filter(projects_measures_data_frame_asnumbers,substr(project, 1, 15)=='2018-2019-tds-a')


# View(p3_2016_2017_projects)
# View(p4_2016_2017_projects)
# View(p3_2017_2018_projects)
# View(p4_2017_2018_projects)
# View(p3_2018_2019_projects)
# View(p4_2018_2019_projects)

p3s <- rbind(
             cbind(group = '2016-2017', p3_2016_2017_projects),
             cbind(group = '2017-2018', p3_2017_2018_projects),
             cbind(group = '2018-2019', p3_2018_2019_projects)
      )

p4s <- rbind(
             cbind(group = '2016-2017', p4_2016_2017_projects),
             cbind(group = '2017-2018', p4_2017_2018_projects),
             cbind(group = '2018-2019', p4_2018_2019_projects)
)

# View(p3s)
# View(p4s)


home <- try(system("echo $HOME", intern = TRUE))
workingDir <- "/select/the/path"

write.csv(projects_measures_data_frame_asnumbers,paste0(home, workingDir,"/data/projects_measures_data_frame_asnumbers.csv"))
write.csv(p3s,paste0(home, workingDir, "/data/p3s.csv"))
write.csv(p4s,paste0(home, workingDir, "/data/p4s.csv"))

#Generate latex tables
library(xtable)

metrics_to_latex <- filter(metrics_data_frame, key %in% metricsKeys) %>% select(name, type)
latex_metrics_table <- xtable(metrics_to_latex, caption='Métricas obtenidas con SonarQube 5.6.6', 
                              label='tab:metrics')
print(latex_metrics_table, file=paste0(home,workingDir,'/tables/metrics.tex'), floating=TRUE)

