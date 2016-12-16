set.seed(100500)
df <- data.frame(group = rep(c("placebo", "treat"), each = 10),
                 pressure = c(rnorm(10, 140, 20) + rnorm(10, 150, 30), 
                              rnorm(10, 135, 30) + rnorm(10, 125, 40))/2)


library(purrr)
library(psych)
res <- split(df, df$group) %>% 
    map(~ describe(.$pressure)) %>% 
    do.call(rbind, .)
kable(res)

shapiro.test(df[df$group == "placebo", 2])

n <- TwoSampleMean.NIS(alpha = 0.025, 
                       beta = 0.2, 
                       sigma = sqrt(res$sd[2]^2 + res$sd[1]^2), 
                       k = 1, 
                       delta = 0, 
                       margin = res$mean[2] - res$mean[1])
ceiling(n)

set.seed(500)
new_sample <- data.frame(group = rep(c("placebo", "treat"), each = 60),
                         pressure = c(sample(df[df$group == "placebo", 2], 
                                             60, replace = TRUE), 
                                      sample(df[df$group == "treat", 2], 
                                             60, replace = TRUE)))

shapiro.test(new_sample[new_sample$group == 
                            "placebo", 2])

library(broom)
t.test(pressure ~ group, data = new_sample) %>% 
    tidy() %>% kable()
wilcox.test(pressure ~ group, data = new_sample) %>% 
    tidy() %>% kable()


reps_expr <- quote(data.frame(group = rep(c("placebo", "treat"), each = 60),
                              pressure = c(sample(df[df$group == "placebo", 2], 
                                                  60, replace = TRUE), 
                                           sample(df[df$group == "treat", 2], 
                                                  60, replace = TRUE))))
    
reps <- replicate(1000, eval(reps_expr), simplify = FALSE)

p_vals <- map_dbl(reps, ~ wilcox.test(pressure ~ group, data = .x)$p.value)

mean(p_vals < 0.05)

p_vals <- map_dbl(reps, ~ t.test(pressure ~ group, data = .x)$p.value)

mean(p_vals < 0.05)



library(boot)
medDif <- function(data, indices) {
    d <- data[indices, ] 
    res <- median(d[d$group == "treat", 2]) - 
           median(d[d$group == "plac", 2])
    return(res)
}


set.seed(600)
new_sample <- data.frame(group = rep(c("placebo", "treat"), each = 60),
                         pressure = c(sample(df[df$group == "placebo", 2], 
                                             60, replace = TRUE), 
                                      sample(df[df$group == "treat", 2], 
                                             60, replace = TRUE)))

results <- boot(data = new_sample, 
                statistic = medDif, 
                R = 1000,
                strata = new_sample$group)
results
plot(results)
boot.ci(results, type = "basic")

