make_attribution_plots <- function(attribution_tab, lob){
  
  att_plot <- attribution_tab %>% 
    gather("fold", "attribution", -channel) %>% 
    filter(channel != "base") %>%
    ggplot(aes(x = channel, y = attribution, group = channel)) +
    geom_boxplot() + coord_flip() + 
    ggtitle(paste(lob, "Attribution Excluding Base"))
  
  base_att_plot <- attribution_tab %>% 
    gather("fold", "attribution", -channel) %>% 
    filter(channel == "base") %>%
    ggplot(aes(x = channel, y = attribution, group = channel)) +
    geom_boxplot() + coord_flip() +
    ggtitle(paste(lob, "Base Attribution"))
  
  return(list(att_plot, base_att_plot))
  
}

make_attribution_summary_table <- function(attribution_tab){
  summary_stats <- attribution_tab %>% 
    gather("fold", "attribution", -channel) %>%
    group_by(channel) %>%
    summarize(mean = mean(attribution),
              sd = sd(attribution),
              median = median(attribution),
              lower_quantile = quantile(attribution, 0.25),
              upper_quantile = quantile(attribution, 0.75),
              max = max(attribution),
              min = min(attribution)) %>%
    mutate(confint_95_lower_bound = mean - sd*(1.96),
           confint_95_upper_bound = mean + sd*(1.96))
  
  return(summary_stats)
}