
#logit

library(vip)
conflict_prefer("vi", "vip")

#padrao para achar os produtos focos
#SearchPattern <- paste(products, collapse = "|")

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
t#ickets2 <- rt_uk %>% 
 # #filter(product %in% products) %>% 
 # select(-stock_code, -invoice_date, -country) %>% 
 # mutate(foco = if_else(str_detect(product, SearchPattern, negate = TRUE),0,1),
 #       #arules_rhs = if_else(str_detect(product, search_rhs, negate = TRUE),0,1),
 #       #arules_lhs = if_else(str_detect(product, search_lhs, negate = TRUE),0,1)
 #       ) %>% 
 # #select(product, foco , arules_rhs, arules_lhs) %>%  #%>% filter(arules_lhs == 1)
 # group_by(invoice_no, date, time, holiday, year, month,
 #          colour, made_from, package) %>%
 # summarise(across(c("quantity", "total_revenue", 
 #                    "foco" #, "arules_rhs", "arules_lhs"
 #                    ), sum)) %>% 
 # arrange(desc(total_revenue)) %>%
 # #summarise(Quantidade = sum(quantity), Receita = sum(total_revenue),
 # #          Foco = sum(foco)) %>% 
 # arrange(desc(total_revenue)) %>%
 # ungroup() %>% 
 # mutate(foco = as.factor(if_else(foco > 0,1,0)), 
 #       #holiday = as.factor(holiday),
 #       week = ceiling(day(date) / 7), #Criacao de Semana e dia da semana
 #       weekday = wday(date),
 #       hour = hour(time)) %>%
 # select(-time) %>% 
 # replace_na(list(colour = "none", made_from = "none", package = "none")) %>% 
 # select(-invoice_no, -date, -year)
#
#skim(tickets2)

#write_csv(tickets2, "output/tickets2.csv")

set.seed(123)
split <- initial_split(tickets2, prop = 0.8)

tr <- training(split)
ts <- testing(split)

#glimpse(tr)

#base para Cross Validation
#cv_splits <- vfold_cv(tr, v = 5, strata = "foco")


#glimpse(tickets2)
#processamento

receita <- recipe(foco ~ ., data = tr) %>% #com os dados de treino
  #update_role(invoice_no, date, new_role = "id") %>% 
  #step_rm(invoice_no, date, year) %>% #retira o user_id e a variável impacto (vamos apenas usar influência)
  step_dummy(made_from, colour, package) %>% 
  step_interact(terms = ~holiday:hour) %>% 
  step_normalize(all_numeric())  #normaliza variaveis numericas

receita_prep <- prep(receita)

treinamento_proc <- bake(receita_prep, new_data = tr)
teste_proc <- bake(receita_prep, new_data = ts) 

doParallel::registerDoParallel(cores = 6)

glm_spec <- logistic_reg() %>%
  set_engine("glm") %>% 
  set_mode("classification") 

#glm_res <- fit_resamples(glm_spec, foco ~ ., cv_splits, 
#                         control = control_resamples(save_pred = TRUE))

#predicted <- tr %>% 
#  mutate(.row = row_number()) %>% 
#  left_join(glm_res %>% 
#              collect_predictions(), by = ".row")
#
#
#write_csv(predicted, "output/predicted.csv")
#
#predicted %>% 
#  filter(foco.y == 1) %>% 
#  na.omit %>% 
#  ggplot(aes(x = wday(weekday, label = T, abbr = T) , 
#             y = hour, fill = .pred_0)) +
#  geom_tile() +
#  scale_fill_viridis_c(labels = scales::percent_format(),
#                       #trans = "log"
#                       ) +
#  #facet_wrap(~ foco.x, nrow = 2) +
#  labs(fill = "Probabilidade de compra", x = "")


glm_fit <- glm_spec %>% 
  fit(foco ~ ., treinamento_proc)


#fitted <- glm_fit %>% 
#  predict(new_data = teste_proc, type = "class")
  
  
  #mutate(predicted = pred_class$foco, 
  #       true = foco)

#view(tidy(glm_fit))

#vip::vip(glm_fit)


#fitted %>% conf_mat(foco ,.pred_class) %>% autoplot()

####################################################

#oParallel::registerDoParallel(cores = 6)

#f <- rand_forest() %>% 
# set_engine("ranger", importance = "impurity") %>% 
# #set_args(keep.inbag = TRUE) %>% 
# set_mode("classification")

#f_fit <- rf %>% 
# fit(Foco ~ ., treinamento_proc)

#ibrary(vip)
#ip(rf_fit)

#itted_rf <- rf_fit %>% 
# predict(new_data = teste_proc, type = "class" ) %>% 
# mutate(observado = teste_proc$Foco, 
#        modelo = "Random Forest")

#itted_rf %>% conf_mat(observado ,.pred_class)


##########################################


#gplot(tickets2, aes(x=date,color=Foco)) + 
# geom_histogram(binwidth=14,fill="white",alpha=0.5, position="identity")


###############################

#oParallel::registerDoParallel(cores = 6)

#esults_test <- glm_fit %>% 
# predict(new_data = teste_proc, type = 'prob') %>% 
# mutate(truth = teste_proc$Foco,
#        model = 'logit')# %>% 
## bind_rows(rf_fit %>% 
##             predict(new_data = teste_proc, type = 'prob') %>% 
##             mutate(truth = teste_proc$Foco,
##                    model = 'random forest',
##                    #correct = case_when(truth == .pred_class ~ "Correct", TRUE ~ "Incorrect")
##             ))
#

#esults_test %>% 
# #group_by(model) %>% 
# #filter(model == c('boosting','random forest') ) %>% 
# roc_curve(truth, .pred_0) %>% 
# autoplot()
#

```