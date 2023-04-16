#Creado por Irvin Rojas
#Para la Incubadora de Evaluaciones 2023
#Datos de ejemplo provenientes de:
#Ruben Irvin Rojas Valdes, Bruce Wydick, Travis J Lybbert, Can hope elevate microfinance? Evidence from Oaxaca, Mexico,
#Oxford Economic Papers, Volume 74, Issue 1, January 2022, Pages 236–264.
#https://academic.oup.com/oep/article-abstract/74/1/236/6154385
#Más detalles en:
#http://oaxacahope-project.weebly.com/

rm(list = ls()) 
options(scipen=999) 

library(tidyverse)
library(sandwich)
library(estimatr) # regresión con errores robustos y agrupados
library(modelsummary) # para hacer tablas
library(ri2) # inferencia por aleatorización


#0. Cargamos datos----
data <- haven::read_dta("data/analysis_dataset.dta") 



#1. El tratamiento se aleatorizó de manera correcta

#Datos de línea base
bl <- data %>% 
  filter(t==0)

summary(bl$educ)

#Por grupos
bl %>% 
  group_by(hopegroup) %>%
  summarize(mean=mean(educ,na.rm=T)) %>% 
  ungroup()

#Prueba de diferencia de medias

t.test(educ ~ hopegroup,
       data = bl)

summary(m1 <- lm(educ ~ hopegroup,
                 data = bl))$coef


#Errores robustos y agrupados
#Nota: CR0 es el estimador de sandwich agrupado de Liang & Zeger (1986) que no hace
#correción de muestras pequeñas
#Algunas modificaciones multiplican CR0 por un factor
#CR1 = CR01 * [G / (G - 1)]
#CR1S = stata =  (G (N-1)) / [(G - 1)(N - k)]



#lm_robust del paquete estimatr
summary(m1r <- lm_robust(educ ~ hopegroup,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = bl))$coef


#Para el resto de los covariables
models <- list()

models[['Edad']] <- lm_robust(age ~ hopegroup,
                                   clusters = communitybank,
                                   se_type = "CR0",
                                   data = bl)
models[['Educación']] <- lm_robust(educ ~ hopegroup,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = bl)
models[['Prop. evangélica']] <- lm_robust(evangelical ~ hopegroup,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = bl)
models[['Hijos']] <- lm_robust(children ~ hopegroup,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = bl)
models[['Líder']] <- lm_robust(bankleader ~ hopegroup,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = bl)

#Todo en una tabla
modelsummary::modelsummary(models,
                           statistic = "std.error",
                           coef_map = c('hopegroup' = "Hope Group"),
                           coef_omit = "Intercept",
                           gof_omit = 'DF|Deviance|R2|AIC|BIC',
                           stars=c('*' = .1, '**' = .05, '***'=0.01),
                           title = "Balance de X en línea base")

#En general, los covariables están bien balanceados













#2. Estimación de efectos de tratamiento----
#Solo errores robustos
summary(e0a <- lm_robust(StdAnderAgencyIndex ~ hopegroup,
                         se_type = "HC1",
                         data = filter(data, t==2)))$coef[,c(1:2,4)]
#Agregamos controles
summary(e0b <- lm_robust(StdAnderAgencyIndex ~ hopegroup + age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index,
                         se_type = "HC1",
                         data = filter(data, t==2)))$coef[,c(1:2,4)]

#Ponemos la ecuación a estimar en un objeto
X <- c("hopegroup", "age", "educ", "evangelical", "children", "children_under_18", "bankleader", "Dwelling_Index")

reglarga <- as.formula(paste("StdAnderAgencyIndex ~ ",
                             paste(X, collapse= "+")))

reglarga


#Errores agrupados
summary(e1 <- lm_robust(reglarga,
                        clusters = communitybank,
                        se_type = "CR0",
                        data = filter(data, t==2)))$coef[,c(1:2,4)]

#Agregar indicadoras de parejas
summary(e2 <- lm_robust(reglarga,
                        fixed_effects = pair_number,
                        clusters = communitybank,
                        se_type = "CR0",
                        data = filter(data, t==2)))$coef[,c(1:2,4)]

modelsummary::modelsummary(list('Errores Robustos'=e0a,
                                'Errores Robustos + X'=e0b,
                                'Errores agrupados + X'=e1,
                                'Errores agrupados + X + parejas'=e2),
                           statistic = "std.error",
                           coef_map = c('hopegroup' = "Hope Group"),
                           coef_omit = "Intercept",
                           gof_omit = 'DF|Deviance|R2|AIC|BIC',
                           stars=c('*' = .1, '**' = .05, '***'=0.01),
                           title = "Efectos de tratamiento en el índice de agencia 12 meses después de la intervención")







#3. ANCOVA----
#Agregar el valor de y en la línea base y un indicador para valores faltantes
X <- c("hopegroup", "age", "educ", "evangelical", "children", "children_under_18", "bankleader", "Dwelling_Index", "Baseline_StdAnderAgencyIndex", "M_Baseline_StdAnderAgencyIndex")

reglarga <- as.formula(paste("StdAnderAgencyIndex ~ ",
                             paste(X, collapse= "+")))

summary(e3 <- lm_robust(reglarga,
                        fixed_effects = pair_number,
                        clusters = communitybank,
                        se_type = "CR0",
                        data = filter(data, t==2)))$coef[,c(1:2,4)]


modelsummary::modelsummary(list('Errores Robustos'=e0a,
                                'Errores Robustos + X'=e0b,
                                'Errores agrupados + X'=e1,
                                'Errores agrupados + X + parejas'=e2,
                                '+ ANCOVA'=e3),
                           statistic = "std.error",
                           coef_map = c('hopegroup' = "Hope Group"),
                           coef_omit = "Intercept",
                           gof_omit = 'DF|Deviance|R2|AIC|BIC',
                           stars=c('*' = .1, '**' = .05, '***'=0.01),
                           title = "Efectos de tratamiento en el índice de agencia 12 meses después de la intervención")




#Resultado en el paper
#Usamos los datos de la ronda de seguimiento de 1 mes y la ronda final al mismo tiempo
summary(epaper <- lm_robust(StdAnderAgencyIndex ~ hopegroup:followup1 + hopegroup:followup2 + followup2 +
                              age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index +
                          factor(pair_number) + factor(type_of_business) +
                          Baseline_StdAnderAgencyIndex +
                          M_Baseline_StdAnderAgencyIndex,
                        clusters = communitybank,
                        se_type = "stata",
                        data = filter(data,t!=0)))





#Hay un efecto en los componentes de la teoría de la esperanza

#Índice de aspiraciones
summary(e4 <- lm_robust(StdAnderAspIndex ~ hopegroup + age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index +
                        Baseline_StdAnderAspIndex +
                        M_Baseline_StdAnderAspIndex,
                        fixed_effects = pair_number,
                        clusters = communitybank,
                        se_type = "CR0",
                        data = filter(data, t==2)))$coef[,c(1:2,4)]

#Índice de avenidas
summary(e5 <- lm_robust(StdAnderAveIndex ~ hopegroup + age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index +
                         Baseline_StdAnderAveIndex +
                         M_Baseline_StdAnderAveIndex,
                         fixed_effects = pair_number,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = filter(data, t==2)))$coef[,c(1:2,4)]

#Hope 3
summary(e6 <- lm_robust(StdAnderHope3Index ~ hopegroup + age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index +
                         Baseline_StdAnderHope3Index +
                         M_Baseline_StdAnderHope3Index,
                         fixed_effects = pair_number,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = filter(data, t==2)))$coef[,c(1:2,4)]

#Índice de desempeño de negocios
summary(e7 <- lm_robust(StdAnderBPIndex ~ hopegroup + age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index +
                         Baseline_StdAnderBPIndex +
                         M_Baseline_StdAnderBPIndex,
                         fixed_effects = pair_number,
                         clusters = communitybank,
                         se_type = "CR0",
                         data = filter(data, t==2)))$coef[,c(1:2,4)]



modelsummary::modelsummary(list('Agencia'=e3,
                                'Aspiraciones'=e4,
                                'Avenidas'=e5,
                                'Hope 3'=e6,
                                'Desempeño negocios'=e7),
                           statistic = "std.error",
                           coef_map = c('hopegroup' = "Hope Group"),
                           coef_omit = "Intercept",
                           gof_omit = 'DF|Deviance|R2|AIC|BIC',
                           stars=c('*' = .1, '**' = .05, '***'=0.01),
                           title = "Efectos de tratamiento en los componentes de la esperanza y el índice de desempeño de negocios")










#4. Inferencia por aleatorización----
set.seed(322)

#Datos de follow-up 2
df <- filter(data, t==2)

#Declaramos la estructura del experimento
declaration <- 
  with(df,{
    declare_ra(
      blocks = pair_number, # bloques = parejas
      clusters = id_g, # clusters o grupos = bancos
      )
  })

#Vemos que entiende lo que pasó
declaration

#Realizamos la asignación aleatoria 1000 veces

ri <- conduct_ri(
  StdAnderAgencyIndex ~ hopegroup,
  assignment = "hopegroup",
  sharp_hypothesis = 0,
  declaration = declaration,
  data = df,
  sims = 1000
)
summary(ri)

#Agregamos controles
ri <- conduct_ri(
  StdAnderAgencyIndex ~ hopegroup +
  age + educ + evangelical + children + children_under_18 + bankleader + Dwelling_Index +
  factor(pair_number) +
  Baseline_StdAnderAgencyIndex +
  M_Baseline_StdAnderAgencyIndex,
  assignment = "hopegroup",
  sharp_hypothesis = 0,
  declaration = declaration,
  data = df,
  sims = 1000
)
summary(ri)


