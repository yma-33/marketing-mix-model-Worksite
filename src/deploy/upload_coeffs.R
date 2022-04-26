library(mmlib)

vertica_setup(server = Sys.getenv("VERTICA_HOST_MMLIB"), ##"aws_qa",
         user = Sys.getenv("mmm_user"),
         password = Sys.getenv("mmm_pass")
)

# create parameter coefficient data frame
df_parameter <- get_query(sql = read_sql("src/deploy/get_para_coeffs.sql"))
mmm_parameter <- data.frame(variable = df_parameter$variable,
                            coeffs = df_parameter$parameterCoeffs,
                            quarter = df_parameter$quarter,
                            process_date = Sys.time(),
                            stringsAsFactors = FALSE)

# create attribution data frame
df_attribution <- get_query(sql = read_sql("src/deploy/get_attribution.sql"))
mmm_attribution <- data.frame(lob = df_attribution$lob,
                              channel = df_attribution$channel,
                              attribution = df_attribution$attribution,
                              quarter = df_attribution$quarter,
                              process_date = Sys.time(),
                              stringsAsFactors = FALSE)

# upload parameter coefficient
upload_df(df = mmm_parameter,
          name = "mmm_parameter_coeffs",
          schema = "models_and_metrics",
          if_exist = "append")

# upload attribution
upload_df(df = mmm_attribution,
          name = "mmm_attribution",
          schema = "models_and_metrics",
          if_exist = "append")

vertica_disconnect()