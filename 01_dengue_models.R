## Load libraries
pacman::p_load("INLA", "dplyr", "lubridate")

# Read in data
data <- read.csv("data/inla_input/data_dengue.csv") %>% subset(Year > 2000 & Year < 2015) %>% 
  dplyr::select(-X) %>% 
  mutate(Date = make_date(Year, Month)) %>% 
  mutate(timestep = rep(1:168, each = 6)) 


avg <- NULL
for (j in unique(data$source)){
  df <- subset(data, data$source == j)
  for (i in df$timestep){
    # 0-2
    df$prcp_02avg[df$timestep == i] = mean(c(subset(df, df$timestep == (i))$prcp, subset(df, df$timestep == (i-1))$prcp, subset(df, df$timestep == (i-2))$prcp))
    df$tmean_02avg[df$timestep == i] = mean(c(subset(df, df$timestep == (i))$tmean, subset(df, df$timestep == (i-1))$tmean, subset(df, df$timestep == (i-2))$tmean))
  }
  avg <- rbind(avg, df)
}


data <- avg %>% subset(Year > 2001 & Year < 2015) 

# Loop through climate sources
dengue_estimates   <- NULL

for (i in unique(data$source)){
  
  df <- data %>% subset(source == i)
  
  y <- df$cases
  e <- df$pop/100000
  
  # Climate effects
  prcp  <- scale(df$prcp_02avg, center = TRUE, scale = TRUE)[,1]             
  tmean <- scale(df$tmean_02avg, center = TRUE, scale = TRUE)[,1] 
  
  # Random effects
  t1 <- as.factor(df$Month) 
  t2 <- as.factor(df$Year)  
  
  df_inla <- data.frame(y, e, t1, t2, tmean, prcp)
  
  formula <- y ~ 1 + f(t1, model = "rw1") + 
    f(t2, model = "iid") + 
    tmean + prcp
  # Model
  mod <- inla(formula, data = df_inla, family = "nbinomial", 
              offset = log(e), verbose = TRUE,
              control.inla = list(strategy = 'adaptive'),
              control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE, 
                                     config = TRUE, 
                                     return.marginals = TRUE), 
              control.predictor = list(link = 1, compute = TRUE), 
              control.family = list(link = "log"))
  save(mod, file = paste0("models/dengue/", i, ".RData"))
  
  # Parameter estimates
  dengue_estimates <- rbind(dengue_estimates, 
                            mod$summary.fixed %>% 
                              dplyr::select(mean, `0.025quant`, `0.975quant`) %>%
                              mutate(variable = rownames(mod$summary.fixed),
                                     source = paste0(i),
                                     model = "Dengue") %>%
                              dplyr::slice(-1) %>% 
                              dplyr::rename(lci = `0.025quant`,
                                            uci = `0.975quant`)) %>%
    tibble::remove_rownames()
  save(dengue_estimates, file = "models/dengue/estimates.RData")
  
  
}
