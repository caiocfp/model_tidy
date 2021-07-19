################################################################################
##pacotes##
################################################################################

pacotes <- c("dplyr","tseries","ggplot2","lmtest","xlsx",
             "FitAR", "tidyverse", "lubridate", 'tidymodels',
             'readr','broom.mixed','dotwhisker',"timetk",
              "modeltime", 'forcats')

lapply(pacotes, library, character.only = T)

rm(pacotes)


###############################################################################
##dados##
################################################################################

DF_MENCOES <- read.csv("(Itaú) Alertas 2018 a 2021.csv",
                       sep = ";")


################################################################################
##dependente##
################################################################################



MENCOES_tbl <- as_tibble(DF_MENCOES %>%
               select(Data.Inicial, Posts) %>%
               set_names(c("date","value")))
                


MENCOES_tbl <- MENCOES_tbl %>% drop_na()

MENCOES_tbl$date  <-  as.Date(MENCOES_tbl$date, format = "%d/%m/%Y")

MENCOES_tbl    


###############################################################################
#Criar espaco para forecast##
##############################################################################
data_nova <- as_tibble(seq(from = as.Date("2021-05-28"), to = as.Date("2021-08-31"), by="days"))

colnames(data_nova) <- c('date')

data_nova$value <- rep(0, n = 96)

data_nova$value <- gsub(0,NA,data_nova$value)


###############################################################################
#acertar o tibble#
###############################################################################
MENCOES_tbl <- as_tibble(rbind.data.frame(MENCOES_tbl,data_nova))

MENCOES_tbl$value <- as.numeric(MENCOES_tbl$value)

################################################################################
##visualizar##
################################################################################

MENCOES_tbl %>%
  plot_time_series(date,value, .interactive = F)


################################################################################
##treino e teste##
################################################################################

splits <- MENCOES_tbl %>% 
          time_series_split(assess = "3 months", cumulative = T)


################################################################################
##Grafico com teste##
################################################################################

splits %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(date, value, .interactive = F)


################################################################################
##modelo autoarima##
################################################################################

model_fit_arima <- arima_reg() %>%
                   set_engine("auto_arima") %>%
                   fit(value ~ date, training(splits))


model_fit_arima


################################################################################
##prophet reg##
################################################################################


model_fit_prophet <- prophet_reg() %>%
                     set_engine("prophet", daily.seasonality = T) %>%
                     fit(value ~ date, training(splits))

model_fit_prophet


################################################################################
##Modelos ML##
################################################################################


##preparar componentes do tempo##

recipe_spec <- recipe(value ~ date, training(splits)) %>%
               step_timeseries_signature(date) %>% 
               step_dummy(all_nominal())


Rc <- recipe_spec %>% prep() %>% juice()


################################################################################
##lASSO##
################################################################################

model_spec_glmnet <- linear_reg(penalty = 0.01, mixture = 1) %>%
                     set_engine("glmnet")   


################################################################################
##Criar um workflow##
################################################################################


workflow_fit_glmnet <- workflow() %>%   ###criar um workflow##
                       add_model(model_spec_glmnet) %>%  ##incluir o modelo##
                       add_recipe(recipe_spec %>% step_rm(date)) %>% ##aqui tem os componentes preparados ##
                       fit(training((splits)))
                       


################################################################################
##Modelos hibridos##
################################################################################

##Prophet Boost##


model_spec_prophet_boost <- prophet_boost() %>%
                            set_engine("prophet_xgboost", daily.seasonality = T)

workflow_fit_prophet_boost <- workflow() %>%
  add_model(model_spec_prophet_boost) %>%
  add_recipe(recipe_spec) %>%
  fit(training(splits))


workflow_fit_prophet_boost


################################################################################
##randon forest##
################################################################################
model_spec_rf <- rand_forest(trees = 100, min_n = 25) %>%
                 set_engine("randomForest")


workflow_fit_rf <- workflow() %>%
                   add_model(model_spec_rf) %>%
                   add_recipe(recipe_spec %>% step_rm(date)) %>%
                   fit(training(splits))





################################################################################
##ModelTime##
################################################################################

################################################################################
##tabela modeltime##
################################################################################

model_table <- modeltime_table(
  model_fit_arima,
  model_fit_prophet,
  workflow_fit_prophet_boost
)

model_table


################################################################################
##calibracao##
################################################################################

calibration_table <- model_table %>%
  modeltime_calibrate(testing(splits))

calibration_table

################################################################################
##Forecast (amostra de teste)##
################################################################################


##predicao####
calibration_table %>% 
  modeltime_forecast(actual_data = MENCOES_tbl) %>%
  plot_modeltime_forecast(.interactive = F)


##acuracia##

calibration_table %>% 
  modeltime_accuracy() %>%
  table_modeltime_accuracy(.interactive = F)

##Grafico##

calibration_table %>% 
modeltime_forecast(actual_data = MENCOES_tbl) %>%
plot_modeltime_forecast(.interactive = F)



##Refit e Forecast##



calibration_table %>%
  modeltime_refit(MENCOES_tbl) %>%
  modeltime_forecast(h = "3 months", actual_data = MENCOES_tbl) %>%
  plot_modeltime_forecast(.interactive = F)

class(MENCOES_tbl)
