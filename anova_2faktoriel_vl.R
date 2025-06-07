library(effectsize)
library(glue)
library(pwr)
library(emmeans)
library(car)



df <- read.csv("/Users/tmarkert/workspace/R/R Clara HS/VL5_2faktoriel.csv", header = TRUE, sep = ",", stringsAsFactors = TRUE)

model = lm(Verbesserung ~ Medikament * Dosis, data = df)

result_anova = anova(model)
result_anova

ES_omega = omega_squared(result_anova)
interpret_omega_squared(ES_omega)

ES_eta = eta_squared(model)
interpret_eta_squared(ES_eta)

emmeans(model, pairwise ~ Medikament)
emmeans(model, pairwise ~ Dosis)



boxplot(df[df$Medikament == "Placebo",]$Verbesserung , df[df$Medikament == "Verum",]$Verbesserung, names = c("Placebo", "Verum"), ylab = "Verbesserung")

boxplot(df[df$Dosis == "niedrig",]$Verbesserung , df[df$Dosis == "hoch",]$Verbesserung, names = c("niedrig", "hoch"), ylab = "Verbesserung")

boxplot(df[df$Medikament == "Placebo" & df$Dosis == "niedrig",]$Verbesserung,
        df[df$Medikament == "Placebo" & df$Dosis == "hoch",]$Verbesserung,
        df[df$Medikament == "Verum" & df$Dosis == "niedrig",]$Verbesserung,
        df[df$Medikament == "Verum" & df$Dosis == "hoch",]$Verbesserung,
        names = c("Placebo-niedrig", "Placebo-hoch", "Verum-niedrig", "Verum-hoch"),
        ylab = "Verbesserung"
       )




