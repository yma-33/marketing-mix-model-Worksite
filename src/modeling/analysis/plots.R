source("src/modeling/startup.R")
source("src/modeling/utils/modeling_utils.R")
source("src/modeling/config_set.R")
source("src/modeling/utils/cross_validation_utils.R")
library("hrbrthemes")
library(scales)
library(ggthemes)
config <- list()
config <- final_config(config)

source("src/modeling/data_preprocessing.R")
lob <- "Ann"
print(lob)
get_optimal_from_config <- paste0(
  lob, "_adstock_ext_factors_", tolower(config$optimize_adstock_with_ext_factors),
  "_multiplicative_", tolower(config$adstock_multiplicative)
)
optimal_adstock_rates <- config[[get_optimal_from_config]]
print(optimal_adstock_rates)

# Get the input dataframe before/after marketing dynamics
colname <- "TV_Brand_NY"
after_adstock <- input[[lob]] %>%
  select(c(Week, column = colname)) %>%
  transform(column = as.numeric(column))
before_astock <- INPUT %>%
  select(c(Week, column = colname)) %>%
  transform(column = as.numeric(column))
after_adstock$Week <- as.Date(after_adstock$Week)
before_astock$Week <- as.Date(before_astock$Week)


# Before and after adstock transformation
after_adstock_manual <- data.frame(Week = INPUT$Week, value = INPUT %>%
  ungroup() %>%
  select(colname) %>%
  adstock_transform_function(0.33))
after_adstock_manual$Week <- as.Date(after_adstock_manual$Week)

# Get before/after bandwidth/delay
d <- 2
bw <- 4
bw_delay_df <- INPUT %>%
  ungroup() %>%
  select(colname)
bw_delay_df[(bw + d):nrow(bw_delay_df), ] <- stats:::filter(bw_delay_df, c(rep(0, d), rep(1, bw)) / bw, side = 1)[(bw + d):nrow(bw_delay_df)]
bw_delay_df <- data.frame(Week=INPUT$Week, value=as.numeric(bw_delay_df$TV_Brand_NY))
bw_delay_df$Week <- as.Date(bw_delay_df$Week)

# Plot
ggplot(before_astock, aes(x = Week, y = column)) +
  ylab("Impressions") +
  ggtitle(expression(atop("Adstock Transformation", atop("adstock rate: 0.33, channel: TV, campaign: Brand, state: NY")))) +
  geom_bar(aes(x = Week, y = column, fill = "Before Adstock"), stat = "identity", color="#002f6c") +
  geom_line(data = after_adstock_2, aes(x = Week, y = value, color = "After Adstock")) +
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
  scale_x_date(
    date_breaks = "3 month", date_minor_breaks = "1 week",
    date_labels = "%b %y"
  ) +
  theme_tufte(base_family = "Montserrat SemiBold") +
  theme(
    axis.text = element_text(color = "#002f6c", size = 10),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, color = "#002f6c", size = 16),
    axis.title.y = element_text(color = "#002f6c", size = 12),
    axis.title.x = element_text(color = "#002f6c", size = 12),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  ) + scale_fill_manual("",values="#002f6c") + 
  scale_colour_manual(
    name = "",
    values = c(
      "Before Adstock" = "#002f6c",
      "After Adstock" = "#009CDE"))

# Get impressions for a specific input combination
cols_to_plot <- c('Audio_Terrestrial_Brand_AL',
                  'Audio_Terrestrial_Sponsorship_AL',
                  "Digital_Video_Brand_AL",
                  "Digital_Video_Direct Response_CA",
                  "Digital_Video_Sponsorship_AZ",
                  "Display_Brand_AZ")
imp_df <- input$Life %>% select("Week",cols_to_plot)
imp_df$Week <- as.Date(imp_df$Week)
imp_df <- imp_df %>% 
            mutate(week_date = as.yearqtr(Week, "%Y/%m/%d")) %>%
            filter(week_date == "2020 Q3") %>%
            select(-c("week_date"))

imp_df_grid <- imp_df %>% pivot_longer(-c(Week))
p <- ggplot(imp_df_grid, aes(Week, value)) +
  geom_bar(stat="identity")
p + facet_wrap(vars(name))

# Get optimized results for each input combination
optimal_results <- read_csv("src/optimization/output/5_13_Q3_2020_30/optimal_m_by_week.csv")
optimal_results <- optimal_results[,-1] %>% 
                      pivot_longer(-c(mnames)) %>%
                      filter(mnames %in% cols_to_plot)
# optimal_results$name <- as.Date(optimal_results$name,"%m/%d/%y")
p <- ggplot(optimal_results, aes(name, value)) +
  geom_bar(stat="identity")
p + facet_wrap(vars(mnames))


# Plot diminishing returns with changing Ms
plot_regression <- data.frame()
for(i in seq(2:1000)) {
  print(i)
  result <- combinationCoeffs %>% 
    mutate(chann = extract_dimension(input, "channel"),
           camp= extract_dimension(input, "campaign"), 
           impr = ifelse(chann=="TV"&camp=="Brand",i,2)) %>%
    mutate(log_term=rlog(impr)) %>%
    summarise(sum(log_term)) %>% pull()
  plot_regression <- rbind(plot_regression,c(i, result))
}

