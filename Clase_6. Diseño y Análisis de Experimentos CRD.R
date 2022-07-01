
################################
# Completely Randomized design
################################

library(tidyverse)
library(desplot)
library(plotly)
library(broom)
library(emmeans)
library(multcomp)
library(multcompView) # mean comparisons
library(readxl)

datos <- read_excel("datos.xlsx")

head(datos)
str(datos)
summary(datos)

# La variedad debe ser definida como factor
datos$Fertilizante <- factor(datos$Fertilizante, 
                             levels = c("Gallinaza", "Cal_Agricola", "Calfos"))
datos$Replica <- as.factor(datos$Replica)

# New Variable
# set.seed(46)
# datos$Production <- c(rnorm(5, 80,  1.4), rnorm(5, 80,  1.4), rnorm(5, 80,  1.4))

# -------------------------------------------------------------------------
# Matrices ----------------------------------------------------------------
# -------------------------------------------------------------------------

# number of levels
t <- nlevels(datos$Fertilizante)

# replication
r1 <- r2 <- r3 <- 5

# total number of observations
n <- r1 + r2 + r3 

# design matrix
mu <- rep(1, n)
lvl_G <- c(rep(1, r1), rep(0, n-r1))
lvl_A <- c(rep(0, r1), rep(1, r2), rep(0, n-(r1+r2)))
lvl_C <- c(rep(0, r1), rep(0, r2), rep(1, r3))

X <- cbind(mu, G = lvl_G, F = lvl_A, A = lvl_C)
X

# rank X
qr(X)$rank

# X'X 
XTX <- t(X) %*% X 
XTX

# rank X'X
qr(X)$rank

# Y
Y <- as.matrix(datos$Production, nrow = n, ncol = 1)
Y

# X'Y
XTY <- t(X) %*% Y
XTY

# adding a new equation
XTX
XTX[1,2:(t+1)] <- 0

# Solution
beta <- solve(XTX, XTY)
beta

# Factor de correccion 
FC <- n*beta[1]^2

# R(beta) = beta'X'Y
R_beta <- t(beta) %*% XTY

# Sum of squares Treatments =  R(Tau|mu) = R_beta - R_mu
SStrt <- R_beta - FC
SStrt

# Sum of squares Total
SST <- t(Y)%*%Y - FC
SST

# Sum of squares Error
SSE <- t(Y - X %*% beta) %*% (Y - X %*% beta)
SSE

# Sigma^2
sigma_2 <- t(Y - X %*% beta) %*% (Y - X %*% beta) / (n - t)
sigma_2

# ANOVA
ANOVA <- data.frame(
  Fuente = c("Fertilizante", "Error", "Total"), 
  DF = c(t - 1 , n - t,  n - 1),
  Sum_Sq = c(SStrt, SSE, SST),
  Mean_Sq = c(SStrt/(t-1), SSE/(n-t), SST/(n-1)),
  F_value = c(SStrt/(t-1) / (SSE/(n-t)), NA, NA ),
  `Pr(>F)` = c( pf( SStrt/(t - 1) / ( SSE/(n-t) ), t - 1 , n - t , lower.tail = F), NA, NA),
  check.names = F
)
ANOVA

# -------------------------------------------------------------------------
# Cheking results ---------------------------------------------------------
# -------------------------------------------------------------------------

ANOVA %>% 
  mutate(source  = c("ANOVA", "ANOVA", "TOTAL"),
         percentage = paste0( round(Sum_Sq/c(SST)*100, 2), "%") ) %>% 
  ggplot(aes(x = source, y = Sum_Sq, fill = Fuente, label = percentage))+
  geom_bar(stat = "identity", color = "black")+
  geom_text(position = position_stack(vjust = 0.5) )+
  theme_bw()+
  theme(legend.position = "top")+
  labs(x = "")

# -------------------------------------------------------------------------
# Chequeo de la variable de respuesta
# -------------------------------------------------------------------------

# summary
means <- datos %>% 
          group_by(Fertilizante) %>% 
          summarize(mean    = mean(Production),
                    std.dev = sd(Production),
                    cv      = std.dev/mean )
means

# points 
g2 <- datos %>% 
  ggplot(aes(x = Fertilizante,  y = Production))+
  geom_point()+
  theme_bw()
g2

# boxplots
g3 <- datos %>% 
  ggplot(aes(x = Fertilizante,  y = Production, fill = Fertilizante))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Production distribution by Fertilizante")
g3

means %>% 
  ggplot(aes(x = Fertilizante, y = mean )) + 
  geom_point(size = 5)+
  theme_bw()+
  ylim(c(min(datos$Production), 
         max(datos$Production)))

# interactive plots
ggplotly(g3)


# -------------------------------------------------------------------------
# Modelling
# -------------------------------------------------------------------------

# model
model <- lm(formula = Production ~ Fertilizante, data = datos )

# ANOVA
anova(model) %>% as.data.frame()
var(datos$Production)*(15-1) # SStotal

# Means by hand
mu <- beta[1]
mu_i <- beta[1] + beta[2:4]

# -------------------------------------------------------------------------
# Confidence intervals ----------------------------------------------------
# -------------------------------------------------------------------------

sigma_2 <- c(sigma_2)
n <- r1 + r2 + r3
r <- 5  
alpha <- 0.05
df <- n - t
t_value <-  qt(p = 1 - alpha/2, df = df)

standard_error_mu <- sqrt( sigma_2 / n )
standard_error_mu_i <- sqrt( sigma_2 / r )

CI_mu <- data.frame(
                "μ" = mu,
                SE = standard_error_mu,
                df = df,
                t_value = t_value,
                "Lower.CL" = mu - t_value*standard_error_mu, 
                "Upper.CL" = mu + t_value*standard_error_mu
                ) %>% 
          mutate_if(is.numeric, round, 2)
CI_mu

CI_mu_i <- data.frame(
                treatment = c("Gallinaza", "Cal_Agricola", "Calfos"),
                mean = mu_i,
                SE = standard_error_mu_i,
                df = df,
                t_value = t_value,
                "Lower.CL" = mu_i - t_value*standard_error_mu_i, 
                "Upper.CL" = mu_i + t_value*standard_error_mu_i
                ) %>% 
          mutate_if(is.numeric, round, 2)
CI_mu_i

# -------------------------------------------------------------------------


# Means 
emmeans(model, ~ Fertilizante)

# Mean comparisons
mean_comparisons <- model %>% 
  emmeans(pairwise ~ "Fertilizante", adjust="tukey") %>% 
  pluck("emmeans") %>% 
  cld(details=TRUE, Letters=letters) # add letter display

mean_comparisons$emmeans # adjusted Fertilizante means


# -------------------------------------------------------------------------
# Plot --------------------------------------------------------------------
# -------------------------------------------------------------------------


# plotting Results
grf <- 
  ggplot() +
  # black dots representing the raw data
  geom_point(
    data = datos,
    aes(y = Production, x = Fertilizante)
  ) +
  # red dots representing the adjusted means
  geom_point(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = Fertilizante),
    color = "red",
    position = position_nudge(x = 0.1)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = mean_comparisons$emmeans,
    aes(ymin = lower.CL, ymax = upper.CL, x = Fertilizante),
    color = "red",
    width = 0.1,
    position = position_nudge(x = 0.1)
  ) +
  # red letters 
  geom_text(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = Fertilizante, label = .group),
    color = "red",
    position = position_nudge(x = 0.2)
  ) + 
  ylim(76, NA) + # force y-axis to start at 0
  ylab("Production in kg/plot") + # label y-axis
  xlab("Fertilizante") +      # label x-axis
  labs(caption = "Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per Fertilizante
       Means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_bw() + # clearer plot format 
  ggtitle("CRD Analysis", subtitle = "UNIVALLE")+
  geom_hline(yintercept = beta[1], linetype = 2) # Overall mean

grf 

ggsave("images/CRD.png",
       plot = grf, 
       units = "in", 
       dpi = 300, 
       width = 5.5, 
       height = 4)

# coord_flip
grf + 
  coord_flip()

# interactive
ggplotly(grf)

# -------------------------------------------------------------------------
# Size of the experiment --------------------------------------------------
# -------------------------------------------------------------------------


tau <- beta[2:4]
r_i <- c(r1, r2, r3)
r <- 5
n <- sum(r_i)
t <- 3
alpha <- 0.05
df_num <- t - 1 
df_denom <- n - t
lambda <- sum(tau^2 * r) / sigma_2
f_critic <- qf( p = 1 - alpha, t - 1, t * r - t, lower.tail = T ) 

power <-  1 - pf(f_critic, t - 1, t * r - t, ncp = sum(tau^2 * r) / sigma_2)


power_function <- function(alpha, r, t, tau, sigma_2){
  f_critic <- qf(p = 1 - alpha, t - 1, t * r - t, lower.tail = T) 
  power <-  1 - pf(f_critic, t - 1, t * r - t, ncp = sum(tau^2 * r) / sigma_2)
  return(power)
}

results <- data.frame(r = 2:10, 
                      power = lapply(2:10,
                                     power_function, 
                                     alpha = alpha,
                                     t = t,
                                     tau = tau,
                                     sigma_2 = sigma_2
                                     ) %>%
                        unlist()
                      )

plot_res <- results %>% 
              ggplot(aes(x = r, y= power))+
              geom_point()+
              geom_line()+
              ylim(c(0,1))+
              theme_bw()+
              geom_vline(xintercept = 5, linetype = 2, color = "red")+
              labs(title = "Sample Size")
plot_res
ggplotly(plot_res)


# Curves 

results <- data.frame(
  expand.grid(r = 2:15,
              sigma_2 = c(1, 1.5, 1.9, 2.5, 3, 3.5),
              alpha = c(0.01, 0.05, 0.10, 0.20)
              )
  )

for (i in 1:nrow(results)) {
  r <- results[i,1]
  sigma_2 <- results[i,2]
  alpha <- results[i,3]
  results[i, "power"] <- power_function(alpha = alpha,
                                        r = r,
                                        t = t,
                                        tau = tau,
                                        sigma_2 = sigma_2)
}

plot_res <- results %>% 
  ggplot(
    aes(x = r, 
        y= power,
        group = sigma_2, 
        color = as.factor(sigma_2)
        )
    )+
  geom_point()+
  geom_line()+
  ylim(c(0,1))+
  theme_bw(base_size = 15)+
  labs(title = "Sample Size",
       y = expression(paste("Power =", 1 - beta)),
       color = expression(paste(sigma[error]^2))
       )+
  facet_wrap(~ alpha, nrow = 1)
plot_res

ggplotly(plot_res)

# -------------------------------------------------------------------------
# LSD ---------------------------------------------------------------------
# -------------------------------------------------------------------------

test <- agricolae::LSD.test(model, "Fertilizante", alpha = 0.05)
test$statistics

LSD_0 <- qt(p = 0.975, df = test$statistics$Df) *
         sqrt( test$statistics$MSerror * ( 1/test$means$r[1] + 1/test$means$r[2] ) )
LSD_0

# Means
mu_i
# Names
names(mu_i) <- c("Gallinaza", "Cal_Agricola", "Calfos")

abs(mu_i["Gallinaza"] - mu_i["Cal_Agricola"]) > LSD_0
abs(mu_i["Gallinaza"] - mu_i["Calfos"]) > LSD_0
abs(mu_i["Cal_Agricola"] - mu_i["Calfos"]) > LSD_0


lsd_test <- function(difference = 0, 
                     alpha = 0.05, 
                     sigma_2 = NULL, 
                     r_1 = NULL,
                     r_2 = NULL, 
                     df = NULL) {
  
  t_value <- qt(p = 1 - alpha/2, df = df) 
  standar_error <- sqrt( sigma_2 * ( 1 / r_1 + 1 / r_2 ) )
  lsd <- t_value * standar_error
  results <- data.frame(diff = difference,
                        abs_diff = abs(difference),
                        t_value = t_value,
                        standar_error = standar_error,
                        lsd = lsd, 
                        lower_CL = difference - lsd,
                        upper_CL = difference + lsd,
                        p_value = pt(abs(difference)/standar_error, df, lower.tail = F),
                        row.names = NULL
                        )
  return(results)
}

lsd_test(
  difference = mu_i["Gallinaza"] - mu_i["Calfos"],
  alpha = 0.05, 
  sigma_2 = sigma(model)^2,
  r_1 = 5,
  r_2 = 5,
  df = 12
  )

differences <- c(mu_i["Gallinaza"] - mu_i["Cal_Agricola"],
                 mu_i["Gallinaza"] - mu_i["Calfos"],
                 mu_i["Cal_Agricola"] - mu_i["Calfos"])

lapply(differences, 
       lsd_test, 
       alpha = 0.05, 
       sigma_2 = sigma(model)^2,
       r_1 = 5,
       r_2 = 5,
       df = 12) %>% 
  data.table::rbindlist()


