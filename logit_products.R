
#logit

library(vip)

#padrao para achar os produtos focos
SearchPattern <- paste(products, collapse = "|")

#direita <- as_tibble(rules.sub@rhs@itemInfo)%>% 
#          filter(!str_detect(labels, numbers)) %>% 
#          pull(labels)

#search_rhs <- paste(direita, collapse = "|")

#esquerda <- as_tibble(rules.sub@lhs@itemInfo) %>% 
#            filter(!str_detect(labels, numbers)) %>% 
#            pull(labels)
#
#search_lhs <- paste(esquerda, collapse = "|")

#criar uma base por Invoice, identificando Invoices com produtos foco
tickets2 <- rt_uk %>% 
  #filter(product %in% products) %>% 
  select(-stock_code, -invoice_date, -country) %>% 
  mutate(foco = if_else(str_detect(product, SearchPattern, negate = TRUE),0,1),
        #arules_rhs = if_else(str_detect(product, search_rhs, negate = TRUE),0,1),
        #arules_lhs = if_else(str_detect(product, search_lhs, negate = TRUE),0,1)
        ) %>% 
  #select(product, foco , arules_rhs, arules_lhs) %>%  #%>% filter(arules_lhs == 1)
  group_by(invoice_no, date, time, holiday, year, month,
           colour, made_from, package) %>%
  summarise(across(c("quantity", "total_revenue", 
                     "foco" #, "arules_rhs", "arules_lhs"
                     ), sum)) %>% 
  arrange(desc(total_revenue)) %>%
  #summarise(Quantidade = sum(quantity), Receita = sum(total_revenue),
  #          Foco = sum(foco)) %>% 
  arrange(desc(total_revenue)) %>%
  ungroup() %>% 
  mutate(foco = as.factor(if_else(foco > 0,1,0)), 
        #holiday = as.factor(holiday),
        week = ceiling(day(date) / 7), #Criacao de Semana e dia da semana
        weekday = wday(date),
        hour = hour(time)) %>%
  select(-time) %>% 
  replace_na(list(colour = "none", made_from = "none", package = "none")) %>% glimpse

#skim(tickets2)

set.seed(123)
split <- initial_split(tickets2, prop = 0.8)

tr <- training(split)
ts <- testing(split)

#glimpse(tr)

#base para Cross Validation
cv_splits <- vfold_cv(tr, v = 5, strata = "foco")


#glimpse(tickets2)
#processamento

receita <- recipe(foco ~ ., data = tr) %>% #com os dados de treino
  update_role(invoice_no, date, new_role = "id") %>% 
  #step_rm(invoice_no, date, year) %>% #retira o user_id e a variável impacto (vamos apenas usar influência)
  step_dummy(colour, #made_from, package
             ) %>% 
  step_interact(terms = ~holiday:hour) %>% 
  step_normalize(all_numeric())  #normaliza variaveis numericas

receita_prep <- prep(receita)

treinamento_proc <- bake(receita_prep, new_data = tr) %>% select(-invoice_no, -date, -year)
teste_proc <- bake(receita_prep, new_data = ts)  %>% select(-invoice_no, -date, -year)


doParallel::registerDoParallel(cores = 6)

glm_spec <- logistic_reg() %>%
  set_engine("glm") %>% 
  set_mode("classification") 

glm_res <- fit_resamples(glm_spec, foco ~ ., cv_splits, 
                         control = control_resamples(save_pred = TRUE))

glm_fit <- glm_spec %>% 
  fit(foco ~ ., treinamento_proc)

fitted <- glm_fit %>% 
  predict(new_data = teste_proc, type = "class" ) %>% 
  mutate(observado = pred_class$Foco, 
         modelo = "logistica")

view(tidy(glm_fit))

vip::vip(glm_fit)


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