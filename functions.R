# Assorted helper functions

pow <- function(effect, n, s2, alpha = .05) {
  se <- sqrt(s2 / sum((1:n - mean(1:n))^2))
  delta <- log(1 + effect/100) / se
  cutoff <- ifelse(effect < 0, qt(p = alpha, df = n - 2), qt(p = 1 - alpha, df = n - 2))
  ifelse(effect < 0, pt(cutoff, df = n - 2, ncp = delta), 1 - pt(cutoff, df = n - 2, ncp = delta))
}
years_pow <- function(effect, power, s2, alpha = .05){
  n <- 3:100
  diff <- Vectorize(pow, "n")(effect, n, s2) - .8
  n[diff > 0][1]
}

pow2 <- function(effect, n, s2, alpha = .05) {
  se <- sqrt(s2 / sum((1:n - mean(1:n))^2))
  delta <- log(1 + effect/100) / se
  cutoff_l <- qt(p = alpha/2, df = n - 2)
  cutoff_u <- qt(p = 1 - alpha/2, df = n - 2)
  pt(cutoff_l, df = n - 2, ncp = delta) + (1 - pt(cutoff_l, df = n - 2, ncp = delta))
}

pretty_ci <- function(ci, loglin = FALSE){
  ci <- as.numeric(ci)
  if (loglin == FALSE){
    paste0("(", signif(ci[1], 2), ", ", signif(ci[2], 2), ")")
  }
  else
  {
    paste0("(", signif((exp(ci[1])-1)*100, 2), ", ", signif((exp(ci[2])-1)*100, 2), ")")
  }
}

coeff_table <- function(data){
  data %>% 
    group_by(station) %>% 
    nest() %>% 
    mutate(fit = map(data, ~lm(log(index) ~ year, data = .x)),
           trend = paste(map_dbl(fit, ~round((exp(coefficients(.x)["year"])-1) * 100, 1)),
                         map_chr(fit, ~pretty_ci(confint(.x)["year", ], loglin = TRUE))),
           s2 = map_dbl(fit, ~summary(.x)$sigma^2),
           df = map_dbl(fit, "df.residual"),
           s2_upper = s2 * df / qchisq(.05, df),
           pow_inc = pow(5, 10, s2),
           pow_inc_lower = pow(5, 10, s2_upper),
           pow_dec = pow(-5, 10, s2),
           pow_dec_lower = pow(-5, 10, s2_upper),
           power_inc = paste0(round(pow_inc, 2), " (", round(pow_inc_lower, 2), ")"),
           power_dec = paste0(round(pow_dec, 2), " (", round(pow_dec_lower, 2), ")"),
           years_inc = years_pow(5, .8, s2),
           years_dec = years_pow(-5, .8, s2)) %>% 
    select(station, trend,  power_dec, power_inc, years_dec, years_inc) %>% 
    ungroup()
}

summary_table <- function(data){
  bind_rows(data %>% group_by(station, study) %>% 
              summarise(days = n(), DPD = sum(dph > 0), DPH = sum(dph)),
            data %>%  group_by(study) %>% 
              summarise(days = n(), DPD = sum(dph > 0), DPH = sum(dph)) %>% 
              mutate(station = "All stations")
  ) %>% 
    mutate(mean.DPH = DPH / days) %>% 
    split(., .$study) %>% 
    reduce(~left_join(.x, .y, by = "station", suffix = c(".SAMBAH", ".SNMP"))) %>% 
    mutate(change.DPH = ifelse(mean.DPH.SAMBAH > 0, 100*(mean.DPH.SNMP/mean.DPH.SAMBAH -1), NA)) %>% 
    select(station, starts_with("days"), starts_with("DPD"), starts_with("mean"), change.DPH) %>% 
    ungroup()
}

make_indices <- function(index_stations, index_years, index_season, response){
  all_stations <- paste0(index_stations, collapse = "+")
  read_csv("imputed_data.csv", col_types = cols(station = col_character())) %>% 
    filter(station %in% index_stations, 
           year %in% index_years, 
           lubridate::month(date) %in% index_season, 
           response_type == response) %>% 
    group_by(station, year, response_type) %>% 
    summarise(index = mean(imputed)) %>% 
    bind_rows(. , group_by(., year, response_type) %>% summarise(index = sum(index)) %>% mutate(station = all_stations)) %>% 
    mutate(station = factor(station, c(index_stations, all_stations))) %>% 
    ungroup()
}

trend_fig <- function(data, y_lab){
  data %>% ggplot(aes(x = year, y = index)) + 
    geom_smooth(method = "glm", method.args = list(family = gaussian(link = "log")), color = "steelblue") + 
    geom_point() + facet_wrap(~station, scales = "free_y")+ theme_bw() + 
    scale_x_continuous(breaks = seq(2012, 2019, by = 2)) + scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    xlab("") + ylab(y_lab)
}
