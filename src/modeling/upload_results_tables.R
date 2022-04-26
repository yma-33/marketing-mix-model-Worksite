#attribution_table_raw <- read.csv("src/data/example_attribution_table.csv") %>% select(-X)
attribution_col_num <- ncol(model_attr)
attribution_table <- model_attr %>% gather(key = "week", value = "attribution", 10:attribution_col_num)

attribution_table <- attribution_table %>% mutate(week = str_sub(week, 2)) 
attribution_table <- attribution_table %>% mutate(week = str_replace(week,"[.]","-"))
attribution_table <- attribution_table %>% mutate(week = str_replace(week,"[.]","-"))

attribution_table <- attribution_table %>% mutate(geo = geo_input) %>% select(-geo_input) %>%
  select(week, channel, campaign, geo, lob, segment, attribution) 

attribution_table[attribution_table$attribution < 0, "attribution"] <- 0

delay_tab <- read_csv("src/data/bandwidth_delay_table.csv")

attribution_table <- attribution_table %>% left_join(delay_tab, by = c("lob", "channel")) %>% select(-bandwidth)

vertica_setup(server = "aws_qa",
              user = Sys.getenv("mmm_user"),
              password = Sys.getenv("mmm_pass"))


upload_df(attribution_table, name = "attribution_table_2022Q2", schema = "mix_media_marketing", if_exists = c("overwrite"))

# upload parameter coeffcients
df_para_coeffs <- data.frame(parameterCoeffs)
df_para_coeffs$variable <- rownames(df_para_coeffs)
df_para_coeffs$quarter <- '2021Q4'
# upload to quarterly temp table for pAPI use
upload_df(df_para_coeffs, name = "para_coeffs", schema = "mix_media_marketing", if_exists = c("overwrite"))
# upload to historical parameter coeffs table
upload_df(df_para_coeffs, name = "para_coeffs_hist", schema = "mix_media_marketing", if_exists = "append")

# upload combination coeffcients
df_comb_coeffs <- combinationCoeffs
df_comb_coeffs$quarter <- '2021Q4'
# upload to quarterly temp table for pAPI use
upload_df(df_comb_coeffs, name = "comb_coeffs", schema = "mix_media_marketing", if_exists = c("overwrite"))
# upload to historical parameter coeffs table
upload_df(df_comb_coeffs, name = "comb_coeffs_hist", schema = "mix_media_marketing", if_exists = "append")

# upload attribution base
df_life_attr <- attr_base$Life
df_life_attr$lob <- 'Life'
df_invs_attr <- attr_base$Invs
df_invs_attr$lob <- 'Invs'
df_ann_attr <- attr_base$Ann
df_ann_attr$lob <- 'Ann'
df_di_attr <- attr_base$Di
df_di_attr$lob <- 'Di'
df_attr <- rbind(df_life_attr, df_invs_attr, df_ann_attr, df_di_attr)
df_attr$quarter <- '2021Q4'
# upload to quarterly temp table for pAPI use
upload_df(df_attr, name = "channel_attribution", schema = "mix_media_marketing", if_exists = c("overwrite"))
# upload to historical parameter coeffs table
upload_df(df_attr, name = "channel_attribution_hist", schema = "mix_media_marketing", if_exists = "append")

