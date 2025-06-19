library(survival)
library(tidyverse)

# Carrega banco
hiv_tuberc <- read_excel("hiv_tuberc.xlsx")


#Remove 4 NAs
hiv_tuberc2 = hiv_tuberc %>%  drop_na()


# Visualiza banco
glimpse(hiv_tuberc2)


# Analise descritiva
hiv_tuberc2 %>% 
  group_by(sex) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))

hiv_tuberc2 %>% 
  group_by(esc) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))

hiv_tuberc2 %>% 
  select(idade) %>% 
  summary()

hiv_tuberc2 %>% 
  group_by(udi) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))

hiv_tuberc2 %>% 
  group_by(sexual) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))

hiv_tuberc2 %>% 
  group_by(candida) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))

hiv_tuberc2 %>% 
  group_by(hemato) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))

hiv_tuberc2 %>% 
  group_by(herpes) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))

hiv_tuberc2 %>% 
  group_by(pneumo) %>% 
  summarise(contagem = n(), .groups = "drop") %>% 
  mutate(propocao = contagem/sum(contagem))


# Curvas de Kaplan-Meier

km_geral <-  survfit(Surv(tempo_tuberc, status_tuberc) ~ 1, data = hiv_tuberc2)

plot(km_geral, xlab = "Dias de acompanhamento", ylab = "S(t) estimado")


#--
km_sexo = survfit(Surv(tempo_tuberc, status_tuberc)~ sex, data = hiv_tuberc2)

plot(km_sexo, col = c("red", "blue"), xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado" )

legend("bottomright",legend = c("Feminino","Masculino"),
       col = c("red", "blue"),
       lty = 1 ,
       bty = "n")


#--
km_esc = survfit(Surv(tempo_tuberc, status_tuberc)~ esc, data = hiv_tuberc2)

plot(km_esc, col = c("red", "blue", "darkgreen", "orange", "purple"),
     xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado")

legend("bottomright",legend = c("0","1", "2", "3", "4"),
       col = c("red", "blue", "darkgreen", "orange", "purple"),
       lty = 1 ,
       bty = "n")
# Suspeita de violar o pressuposto de riscos proporcionais

cox.zph(coxph(Surv(tempo_tuberc, status_tuberc) ~ esc, data = hiv_tuberc2))

# Não viola
#--
km_udi = survfit(Surv(tempo_tuberc, status_tuberc)~ udi, data = hiv_tuberc2)

plot(km_udi, col = c("blue", "darkgreen"),
     xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado")
legend("bottomright",legend = c("0","1"),
       col = c( "blue", "darkgreen"),
       lty = 1 ,
       bty = "n")
# Suspeita de violar o pressuposto de riscos proporcionais

cox.zph(coxph(Surv(tempo_tuberc, status_tuberc) ~ udi, data = hiv_tuberc2))

# Não viola

#--
km_sexual = survfit(Surv(tempo_tuberc, status_tuberc)~ sexual, data = hiv_tuberc2)

plot(km_sexual, col = c("blue", "darkgreen"),
     xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado")

legend("bottomright",legend = c("0","1"),
       col = c( "blue", "darkgreen"),
       lty = 1 ,
       bty = "n")

#--
km_candida = survfit(Surv(tempo_tuberc, status_tuberc)~ candida, data = hiv_tuberc2)

plot(km_candida, col = c("blue", "darkgreen"),
     xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado")

legend("bottomright",legend = c("0","1"),
       col = c( "blue", "darkgreen"),
       lty = 1 ,
       bty = "n")

#--
km_hemato = survfit(Surv(tempo_tuberc, status_tuberc)~ hemato, data = hiv_tuberc2)

plot(km_hemato, col = c("blue", "darkgreen"),
     xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado")

legend("bottomright",legend = c("0","1"),
       col = c( "blue", "darkgreen"),
       lty = 1 ,
       bty = "n")

#--
km_herpes = survfit(Surv(tempo_tuberc, status_tuberc)~ herpes, data = hiv_tuberc2)

plot(km_herpes, col = c("blue", "darkgreen"),
     xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado")

legend("bottomright",legend = c("0","1"),
       col = c( "blue", "darkgreen"),
       lty = 1 ,
       bty = "n")

#--
km_pneumo = survfit(Surv(tempo_tuberc, status_tuberc)~ pneumo, data = hiv_tuberc2)

plot(km_pneumo, col = c("blue", "darkgreen"),
     xlab = "Dias de acompanhamento",
     ylab = "S(t) estimado")

legend("bottomright",legend = c("0","1"),
       col = c( "blue", "darkgreen"),
       lty = 1 ,
       bty = "n")

#modelagem de cox estratificada

# teste de pressuposto para a varivavel continua

cox.zph(coxph(Surv(tempo_tuberc, status_tuberc) ~ idade, data = hiv_tuberc2))

# Viola o pressuposto de riscos proporcionais

fit_cheio <- coxph(Surv(tempo_tuberc, status_tuberc)~ 
                     factor(sex) + factor(esc) + strata(idade) +
                     factor(udi) + factor(sexual) +
                     factor(candida) + factor(herpes) + 
                     factor(pneumo)+ factor(hemato), method="breslow", data=hiv_tuberc2)

summary(fit_cheio)

fit_final <- coxph(Surv(tempo_tuberc, status_tuberc)~ 
                     factor(sex) +
                     factor(candida) + factor(herpes) + 
                     factor(pneumo)+ factor(hemato), method="breslow", data=hiv_tuberc2)
summary(fit_final)
