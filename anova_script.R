library(effectsize)
library(glue)
library(pwr)
library(emmeans)
library(car)

# Daten aus den Stickproben
#Placebo = c(6, 7, 7, 8)
#Medikament.1 = c(10, 11, 11, 12)
#Medikament.2 = c(11, 12, 12, 13)

# Beispiel-Daten erzeugen
set.seed(123)
n = 50
k = 3
N = n*k

mean = 7.14; sd = 3.7
sample = rnorm(n, mean = mean, sd = sd)
Placebo = (sample - mean(sample)) / sd(sample) * sd + mean

mean = 11.59; sd = 3.62
sample = rnorm(n, mean = mean, sd = sd)
Medikament.1 = (sample - mean(sample)) / sd(sample) * sd + mean

mean = 10.98; sd = 3.96
sample = rnorm(n, mean = mean, sd = sd)
Medikament.2 = (sample - mean(sample)) / sd(sample) * sd + mean

# Daten zusammenfassen
Symptome <- c(Placebo, Medikament.1, Medikament.2)
Symptome_Names = c("Placebo", "Medikament.1", "Medikament.2")
Gruppe <- factor(rep(Symptome_Names, each = n))

# Daten in ein Dataframe packen
df <- data.frame(Symptome, Gruppe)

# Einfaktorielle ANOVA durchführen
result_anova = anova(lm(Symptome ~ Gruppe,
                        data = df[df$Gruppe == "Placebo" | df$Gruppe == "Medikament.1",]))
result_anova
omega_squared(result_anova)
summary(result_anova)
# --> F-Wert = 36.953

# ANOVA und t-test liefern das gleiche Ergebnis  (p-Wert)
t.test(Symptome ~ Gruppe,
       data = df[df$Gruppe == "Placebo" | df$Gruppe == "Medikament.1",],
       var.equal = TRUE)
# --> t-Wert = 6.0789 (t^2 = F; 6.0789^2 = 36.953)

boxplot(df[df$Gruppe == "Placebo",]$Symptome, df[df$Gruppe == "Medikament.1",]$Symptome, df[df$Gruppe == "Medikament.2",]$Symptome, names = Symptome_Names, notch = TRUE)

# Einfaktorielle ANOVA durchführen
result_anova = anova(lm(Symptome ~ Gruppe,
                        data = df[df$Gruppe == "Placebo" | df$Gruppe == "Medikament.1" | df$Gruppe == "Medikament.2",]))
result_anova
omega_squared(result_anova)

# Manually calculated omega^2
f2 = (result_anova$`F value`[1] - 1) * result_anova$Df[1] / N
glue("f2 = {f2}")
om2 = f2 / (1 + f2)
glue("om2 = {om2}")

# eta2 calculated by sum squared
eta2 = result_anova$`Sum Sq`[1] / (result_anova$`Sum Sq`[1] + result_anova$`Sum Sq`[2])
eta2
# eta2 calculated by fs2
fs2 = result_anova$`F value`[1] * result_anova$Df[1] / result_anova$Df[2]
om2 = fs2 / (1 + fs2)
om2

# Stichprobenumfangsplanung

pwr.anova.test(k=3, f= 0.2, sig.level = 0.05, power = 0.9)

# Tukey HSD - Mittelwertsvergleich
emmeans(lm(Symptome ~ Gruppe, data = df), pairwise ~ Gruppe, adjust = "tukey")

# Bonferroni - Mittelwertsvergleich
emmeans(lm(Symptome ~ Gruppe, data = df), pairwise ~ Gruppe, adjust = "bonferroni")

# Benjamini & Hochberg (aka False Discovery Rate Correction, FDR)
emmeans(lm(Symptome ~ Gruppe, data = df), pairwise ~ Gruppe, adjust = "fdr")

# Varianzhomogenität
# wenn Levene Test signifikant, dann liegt keine Varianzhomogenität vor
leveneTest(Symptome ~ Gruppe, data = df, center = "mean")

# Normalverteilung liegt vor?
# Ein signifikanter Shapiro-Wilk Test weist auf Verletzung der NV-Annahme hin
by(df$Symptome, df$Gruppe, shapiro.test)


# ----------------------------------------
# X = c(17, 22, 44, 25, 25, 17, 32, 54, 21, 54, 12, 34, 71, 41, 23, 43, 63, 60, 51, 57, 55, 40, 65, 45, 49, 54)
# Y = c(60, 70, 39, 46, 61, 36, 60, 72, 48, 64, 23, 92, 88, 60, 31, 59, 97, 85, 98, 73, 89, 85, 97, 71, 90, 83)