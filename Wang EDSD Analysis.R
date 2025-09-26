library(mice)
library(dplyr)
library(naniar)
library(tidyverse)
library(rio)
library(foreign)
library(tidylog)
library(Hmisc)
library(jtools)
library(performance)  
library(see)    
library(patchwork) 
library(qqplotr)
library(plm)
library(ggplot2)
library(stargazer)

#load the data
setwd("/Users/hkwang9/Library/CloudStorage/OneDrive-TheUniversityofChicago/University of Chicago/Academics/MA Thesis/3 Data")

edd <- import("European Militarization Dataset.dta")
edd1 <- edd #make a copy of an original data 

#begin the ana;ysis 
edd <- edd %>% 
  arrange(year, ccode)
#structure the data into panel data 
panel_edd <- pdata.frame(edd, index = c("ccode", "year"))

#the signaling model 
s1 <- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons, data = panel_edd, model = "pooling")
summary(s1)

#the signaling model with country-fixed effects 
s2 <- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons, data = panel_edd, model = "within")
summary(s2)

# Plot each predictor vs the response
p1 <- ggplot(edd, aes(x = armsimp, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  scale_x_continuous(limits = c(0, 5)) + 
  labs(title = "rdef vs armsimp")

p2 <-ggplot(edd, aes(x = armsexp, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "rdef vs armsexp")

p3 <- ggplot(edd, aes(x = personnel, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "rdef vs personnel")

p4 <-ggplot(edd, aes(x = weapons, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  scale_x_continuous(limits = c(0, 5)) +
  labs(title = "rdef vs weapons")

combined_plot_signaling <- p1 + p2 + p3 + p4 + plot_layout(ncol = 4)
combined_plot_signaling 

#full signaling model 
s3 <- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "pooling")
summary(s3)

#full signaling model with country-fixed effects 
s4 <- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "within")
summary(s4)

#norms model 
n1 <- plm(formula = rdef ~ log10(refugees) + unpk + bi_oda + unscres, data = panel_edd, model = "pooling")
summary(n1)

#norms model with country fixed effects 
n2 <- plm(formula = rdef ~ log10(refugees) + unpk + bi_oda + unscres, data = panel_edd, model = "within")
summary(n2)

# Plot each predictor vs the response
p5 <- ggplot(edd, aes(x = log_10_ref, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(title = "rdef vs log10(refugees)")

p6 <-ggplot(edd, aes(x = unpk, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(title = "rdef vs unpk")

p7 <- ggplot(edd, aes(x = bi_oda, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(title = "rdef vs BiOda")

p8 <-ggplot(edd, aes(x = unscres, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(title = "rdef vs UNSCRes")

combined_plot_norms <- p5 + p6 + p7 + p8 + plot_layout(ncol = 4)
combined_plot_norms 

#full norms model 
n3 <- plm(formula = rdef ~ log10(refugees) + unpk + bi_oda + unscres + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "pooling")
summary(n3)

#full norms model with country fixed effects 
n4 <- plm(formula = rdef ~ log10(refugees) + unpk + bi_oda + unscres + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "within")
summary(n4)

#colonial ties model 
ct1 <- plm(formula = rdef ~ involvement + colconflict + fdi, data = panel_edd, model = "pooling")
summary(ct1)

#colonial ties model with country fixed effects 
ct2 <- plm(formula = rdef ~ involvement + colconflict + fdi, data = panel_edd, model = "within")
summary(ct2)

#plot rdef vs. predictors
p9 <- ggplot(edd, aes(x = involvement, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(title = "rdef vs involvement")

p10 <-ggplot(edd, aes(x = colconflict, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(title = "rdef vs colconfclict")

p11 <- ggplot(edd, aes(x = fdi, y = rdef)) +
  geom_point(alpha = 0.6, color = "navy") +
  geom_smooth(method = "lm", se = TRUE, color = "maroon") +
  labs(title = "rdef vs fdi")

combined_plot_ct <- p9 + p10 + p11 + plot_layout(ncol = 3)
combined_plot_ct

#colonial ties full model 
ct3 <- plm(formula = rdef ~ involvement + colconflict + fdi + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "pooling")
summary(ct3)
#colonial ties full model with country fixed effects 
ct4 <- plm(formula = rdef ~ involvement + colconflict + fdi + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "within")
summary(ct4)  

#full model without controls 
f1<- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons + log10(refugees) + unpk + bi_oda + unscres + involvement + colconflict + fdi, data = panel_edd, model = "pooling")
summary(f1)

#full model without controls & with fixed effects 
f2 <- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons + log10(refugees) + unpk + bi_oda + unscres + involvement + colconflict + fdi, data = panel_edd, model = "within") 
summary(f2)

#full model 
f3 <- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons + log10(refugees) + unpk + bi_oda + unscres + involvement + colconflict + fdi + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "pooling")
summary(f3)
#full model with country fixed effects 
f4 <- plm(formula = rdef ~ armsimp + armsexp + personnel + weapons + log10(refugees) + unpk + bi_oda + unscres + involvement + colconflict + fdi + eu + fri + dem + nato + intrastcon + uspres, data = panel_edd, model = "within")
summary(f4)

#results using multiple imputations 
# signaling model 
edd_signaling <- edd %>% select(rdef, eu, fri, dem, nato, intrastcon, uspres, armsimp, armsexp, personnel, weapons) 
imp_edd_signaling <- mice(edd_signaling, m=15, seed = 12345)

imp_sig_model <- with(imp_edd_signaling, lm(rdef ~ armsimp + armsexp + personnel + weapons))
summary(imp_sig_model)
pool(imp_sig_model)
summary(pool(imp_sig_model))

imp_sig_model_full <- with(imp_edd_signaling, lm(rdef ~ eu + fri + dem + nato + intrastcon + uspres + armsimp + armsexp + personnel + weapons))
summary(imp_sig_model_full, digit = 3)
pool(imp_sig_model_full)
summary(pool(imp_sig_model_full))

#norms model 
edd_norms <- edd %>% select(rdef, eu, fri, dem, nato, intrastcon, uspres, log_10_ref, unpk, bi_oda, unscres)
imp_edd_norms <- mice(edd_norms, m = 15, seed = 12345)

imp_norms_model <- with(imp_edd_norms, lm(rdef ~ log_10_ref + unpk + bi_oda + unscres))
summary(imp_norms_model)
pool(imp_norms_model)
summary(pool(imp_norms_model))

imp_norms_model_full <- with(imp_edd_norms, lm(rdef ~ log_10_ref + unpk + bi_oda + unscres + eu + fri + dem + nato + intrastcon + uspres))
summary(imp_norms_model_full, digit = 3)
pool(imp_norms_model_full)
summary(pool(imp_norms_model_full))

#colonial ties 
edd_ct <- edd %>% select(rdef, eu, fri, dem, nato, intrastcon, uspres, involvement, colconflict, fdi)
imp_edd_ct <- mice(edd_ct, m=15, seed = 12345)

imp_ct <- with(imp_edd_ct, lm(rdef ~ involvement + colconflict + fdi))
summary(imp_ct, digit = 3)
pool(imp_ct)
summary(pool(imp_ct))

imp_ct_full <- with(imp_edd_ct, lm(rdef ~ involvement + colconflict + fdi + eu + fri + dem + nato + intrastcon + uspres))
summary(imp_ct_full, digit = 3)
pool(imp_ct_full)
summary(pool(imp_ct_full))

#full model 
full <- edd %>% select(rdef, eu, fri, dem, nato, intrastcon, uspres, armsimp, armsexp, personnel, weapons, log_10_ref, unpk, bi_oda, unscres, involvement, colconflict, fdi)
imp_full <- mice(full, m=15, seet = 12345)

imp_fullmodel <- with(imp_full, lm(rdef ~ armsimp + armsexp + personnel + weapons + log_10_ref + unpk + bi_oda + unscres + involvement + colconflict + fdi + eu + fri + dem + nato + intrastcon + uspres))
summary(imp_fullmodel, digit = 3)
pool(imp_fullmodel)
summary(pool(imp_fullmodel))

#sewing the regression results together 
#first evaluation: just explanatory variables 
stargazer(s1, n1, ct1, f1, type = "html", column.labels = c("Signaling", "Norms", "Colonial Ties", "Full"), column.separate = c(1, 1, 1, 1), out = "table2.doc") 
stargazer(s2, n2, ct2, f2, type = "html", column.labels = c("Signaling", "Norms", "Colonial Ties", "Full"), column.separate = c(1, 1, 1, 1), out = "table3.doc")
stargazer(s3, n3, ct3, f3, type = "html", column.labels = c("Signaling", "Norms", "Colonial Ties", "Full"), column.separate = c(1, 1, 1, 1), out = "table4.doc")
stargazer(s4, n4, ct4, f4, type = "html", column.labels = c("Signaling", "Norms", "Colonial Ties", "Full"), column.separate = c(1, 1, 1, 1), out = "table5.doc")


