library(tidyverse)
library(tidymodels)

# following https://www.brodrigues.co/blog/2018-11-25-tidy_cv/

model_df = readRDS("working/model_df.rds")

# train test con tidymodels ----

train_test_split = initial_split(model_df, prop = 0.9)
df_train = training(train_test_split)
df_test  = testing(train_test_split)


# validation ----

# supongo que es como CV pero haciendo 'times' sampleos donde train es 90% y validation es 10%
# no garantiza usar todos los datos al no ser 'secuencial' ?

validation_data =  mc_cv(df_train, prop = 0.9, times = 30)


# recipe ----
simple_recipe <- function(dataset){
  recipe(w_dummy ~ ., data = dataset) %>%
    step_center(all_numeric()) %>% # centra a la media
    step_scale(all_numeric())   # divide por SD
    #step_dummy(all_nominal())      # convierte en dummy lo nominal
}


# aplicar recipe a test ----

# no termino de entender lo de testing.
testing_rec = prep(simple_recipe(df_test), testing = df_test)
test_data   = bake(testing_rec, new_data = df_test) # dataset con las transformaciones


# lugar para cuando aplique recipe a CV "validation_data" ----


# aplicar recipe a train ----

training_rec = prep(simple_recipe(df_train), testing = df_train)
train_data   = bake(training_rec, new_data = df_train)

# logistic ----

# preparamos modelo
log_mod = parsnip::logistic_reg(
                      mode = "classification",
                      penalty = NULL,
                      mixture = NULL)

# fiteamos
log_fit = parsnip::fit(
                    object = log_mod,
                    formula = w_dummy ~ .,
                    data    = train_data)


# results logistic ----

results = tibble(
  actual = test_data$w_dummy,
  predicted = parsnip::predict.model_fit(log_fit, test_data, type = "class")
) %>%
  mutate(predicted= as.factor(pull(predicted)))


# accuracy logistic ----
metrics(results, truth = actual, estimate = predicted) %>% 
  knitr::kable()


conf_mat(results, truth = actual, estimate = predicted)[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(Prediction, Truth, alpha = n)) + 
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), color = "white", alpha = 1, size = 8) +
  labs(
    title = "Confusion matrix"
  )

log_fit$fit$coefficients %>%
  as.data.frame() %>%
  arrange(desc(.))


# random forest

rf_mod <- 
  rand_forest(
    mode = "classification",
    trees = 1000
  )

## Fitting
rf_fit <- 
  fit(
    object = rf_mod,
    formula = w_dummy ~ .,
    data = train_data,
    engine = "randomForest"
  )


# results RF ----

results = tibble(
  actual = test_data$w_dummy,
  predicted = parsnip::predict.model_fit(rf_fit, test_data, type = "class")
) %>%
  mutate(predicted= as.factor(pull(predicted)))

# accuracy RF ----
metrics(results, truth = actual, estimate = predicted) %>% 
  knitr::kable()


conf_mat(results, truth = actual, estimate = predicted)[[1]] %>% 
  as_tibble() %>% 
  ggplot(aes(Prediction, Truth, alpha = n)) + 
  geom_tile(show.legend = FALSE) +
  geom_text(aes(label = n), color = "white", alpha = 1, size = 8) +
  labs(
    title = "Confusion matrix"
  )


