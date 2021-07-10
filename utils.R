# load libraries
library("ggplot2")
library("viridis")

genSubset <- function(dat, nTrain) {
  my.sub.ind <- sample(nrow(dat), nTrain)
  return(dat[my.sub.ind,])
}

# define graphics theme
my_theme = function(legend.position='right'){
  theme_bw() %+replace%
    theme(legend.position=legend.position)
}

theme_set(my_theme())


default_color = "azure4"


#' Make character vector pretty
pretty_rownames = function(rnames){
  rnames = gsub('^`', '', rnames)
  rnames = gsub('`$', '', rnames)
  rnames = gsub('`', ':', rnames)
  rnames
}


year_diff = function(date1, date2){
  day_diff(date1, date2) / 365.25
}

day_diff = function(date1, date2){
  as.numeric(difftime(as.Date(date1), as.Date(date2), units = 'days'))
}

clean.bike.data = function(bike){
  
  bike$weekday = factor(bike$weekday, levels=0:6, labels = c('SUN', 'MON', 'TUE', 'WED', 'THU', 'FRI', 'SAT'))
  bike$holiday = factor(bike$holiday, levels = c(0,1), labels = c('NO HOLIDAY', 'HOLIDAY'))
  bike$workingday = factor(bike$workingday, levels = c(0,1), labels = c('NO WORKING DAY', 'WORKING DAY'))
  bike$season = factor(bike$season, levels = 1:4, labels = c('WINTER','SPRING', 'SUMMER', 'FALL'))
  bike$weathersit = factor(bike$weathersit, levels = 1:3, labels = c('GOOD', 'MISTY', 'RAIN/SNOW/STORM'))
  bike$mnth = factor(bike$mnth, levels = 1:12, labels = c('JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN', 'JUL', 'AUG', 'SEP', 'OKT', 'NOV', 'DEZ'))
  bike$yr[bike$yr == 0] = 2011
  bike$yr[bike$yr == 1] = 2012
  bike$yr = factor(bike$yr)
  bike$days_since_2011 = day_diff(bike$dteday, min(as.Date(bike$dteday)))
  
  # denormalize weather features:
  # temp : Normalized temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-8, t_max=+39 (only in hourly scale)
  bike$temp = bike$temp * (39 - (-8)) + (-8)
  # atemp: Normalized feeling temperature in Celsius. The values are derived via (t-t_min)/(t_max-t_min), t_min=-16, t_max=+50 (only in hourly scale)
  bike$atemp = bike$atemp * (50 - (16)) + (16)
  
  #windspeed: Normalized wind speed. The values are divided to 67 (max)
  bike$windspeed = 67 * bike$windspeed
  #hum: Normalized humidity. The values are divided to 100 (max)
  bike$hum = 100 * bike$hum
  
  
  dplyr::select(bike, -instant, -dteday, -registered, -casual, -atemp)
}

effect_plot = function(mod, dat,  feature_names=NULL){
  X = get_effects(mod, dat)
  if(!missing(feature_names)){
    rownames(X) = feature_names
  }
  X = tidyr::gather(X)
  require("ggplot2")
  ggplot(X) +
    geom_hline(yintercept=0, linetype=4) +
    geom_boxplot(aes(x=key, y=value, group=key)) +
    coord_flip() +
    scale_y_continuous('Feature effect') +
    my_theme()
}


#' Plot coefficients of a linear model
coef_plot = function(mod, alpha = 0.05, remove_intercept = TRUE){
  lm_summary = summary(mod)$coefficients
  rownames(lm_summary) = pretty_rownames(rownames(lm_summary))
  
  df = data.frame(Features = rownames(lm_summary),
                  Estimate = lm_summary[,'Estimate'],
                  std_error = lm_summary[,'Std. Error'])
  df$lower = df$Estimate - qnorm(alpha/2) * df$std_error
  df$upper = df$Estimate + qnorm(alpha/2) * df$std_error
  
  
  if(remove_intercept){
    df = df[!(df$Features == '(Intercept)'),]
  }
  require("ggplot2")
  ggplot(df) +
    geom_vline(xintercept=0, linetype=4) +
    geom_point(aes(x=Estimate, y=Features)) +
    geom_segment(aes(y=Features, yend=Features, x=lower, xend=upper), arrow = arrow(angle=90, ends='both', length = unit(0.1, 'cm'))) +
    scale_x_continuous('Weight estimate') +
    my_theme()
}
