
#logit

#padrao para achar os produtos focos
SearchPattern <- paste(products, collapse = "|")

#criar uma base por Invoice, identificando Invoices com produtos foco
tickets2 <- rt_uk %>% 
  mutate(foco = if_else(str_detect(description, SearchPattern, negate = TRUE),0,1)) %>% 
  group_by(invoice_no, date, time, holiday, year, month,
           colour, made_from, package, ) %>%
  summarise(Quantidade = sum(quantity), Receita = sum(total_revenue),
            Foco = sum(foco)) %>% 
  arrange(desc(Receita)) %>%
  ungroup() %>% 

  mutate(Foco = as.factor(if_else(Foco >0,1,0)), 
        #holiday = as.factor(holiday),
        week = ceiling(day(date) / 7), #Criacao de Semana e dia da semana
        weekday = wday(date),
        hour = hour(time)) %>%
  select(-time) %>% 
  replace_na(list(colour = "none", made_from = "none", package = "none"))

skim(tickets2)

set.seed(123)
split <- initial_split(tickets2, prop = 0.8)

tr <- training(split)
ts <- testing(split)

glimpse(tr)

#base para Cross Validation
cv_split <- vfold_cv(tr, v = 10, strata = "Foco")


glimpse(tickets2)
#processamento
receita <- recipe(Foco ~ ., data = tr) %>% #com os dados de treino
  update_role(invoice_no, date, new_role = "id") %>% 
  #step_rm(invoice_no,date,year) %>% #retira o user_id e a variável impacto (vamos apenas usar influência)
  step_dummy(colour, made_from, package) %>% 
  step_interact(terms = ~holiday:hour) %>% 
  step_normalize(all_predictors())  #normaliza variaveis numericas

receita_prep <- prep(receita)

treinamento_proc <- bake(receita_prep, new_data = tr)
teste_proc <- bake(receita_prep, new_data = ts)

glm_fit <- logistic_reg() %>% 
  set_engine("glm") %>%
  set_mode("classification") %>% 
  fit(Foco ~ ., treinamento_proc)

tidy(glm_fit)

fitted <- glm_fit %>% 
  predict(new_data = teste_proc, type = "class" ) %>% 
  mutate(observado = teste_proc$Foco, 
         modelo = "logistica")


fitted %>% conf_mat(observado ,.pred_class) %>% autoplot()

####################################################

doParallel::registerDoParallel(cores = 6)

rf <- rand_forest() %>% 
  set_engine("ranger", importance = "impurity") %>% 
  #set_args(keep.inbag = TRUE) %>% 
  set_mode("classification")

rf_fit <- rf %>% 
  fit(Foco ~ ., treinamento_proc)

library(vip)
vip(rf_fit)

fitted_rf <- rf_fit %>% 
  predict(new_data = teste_proc, type = "class" ) %>% 
  mutate(observado = teste_proc$Foco, 
         modelo = "Random Forest")

fitted_rf %>% conf_mat(observado ,.pred_class)


##########################################


ggplot(tickets2, aes(x=date,color=Foco)) + 
  geom_histogram(binwidth=14,fill="white",alpha=0.5, position="identity")


###############################

doParallel::registerDoParallel(cores = 6)

results_test <- glm_fit %>% 
  predict(new_data = teste_proc, type = 'prob') %>% 
  mutate(truth = teste_proc$Foco,
         model = 'logit')# %>% 
 # bind_rows(rf_fit %>% 
 #             predict(new_data = teste_proc, type = 'prob') %>% 
 #             mutate(truth = teste_proc$Foco,
 #                    model = 'random forest',
 #                    #correct = case_when(truth == .pred_class ~ "Correct", TRUE ~ "Incorrect")
 #             ))
#



results_test %>% 
  #group_by(model) %>% 
  #filter(model == c('boosting','random forest') ) %>% 
  roc_curve(truth, .pred_0) %>% 
  autoplot()


```