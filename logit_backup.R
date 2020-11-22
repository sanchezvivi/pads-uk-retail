```{r}
#logit

#padrao para achar os produtos focos
SearchPattern <- paste(products, collapse = "|")

#criar uma base por Invoice, identificando Invoices com produtos foco
tickets2 <- rt_uk %>% mutate(foco = if_else(str_detect(description, SearchPattern, negate = TRUE),0,1)) %>% 
  group_by(invoice_no,date,time,holiday,year,month) %>%
  summarise(Quantidade = sum(quantity), Receita = sum(total_revenue), Foco = sum(foco)) %>% 
  arrange(desc(Receita)) %>% mutate(Foco = as.factor(if_else(Foco >0,1,0))) %>% 
  mutate(holiday = as.factor(holiday))

#Criacao de Semana e dia da semana
tickets2 <- tickets2 %>% mutate(week = ceiling(day(date) / 7)) %>% 
  mutate(weekday = weekdays(date))

set.seed(123)
split <- initial_split(tickets2, prop = 0.8)

tr <- training(split)
ts <- testing(split)

#base para Cross Validation
cv_split <- vfold_cv(tr, v = 10, strata = "Foco")

#processamento
receita <- recipe(Foco ~ ., data = tr) %>% #com os dados de treino
  step_rm(invoice_no,date,year) %>% #retira o user_id e a variável impacto (vamos apenas usar influência)
  step_normalize(Quantidade,Receita)  #normaliza variaveis numericas

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


fitted %>% conf_mat(observado ,.pred_class)

####################################################

rf <- rand_forest() %>% 
  set_engine("ranger", importance = "permutation") %>% 
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
```