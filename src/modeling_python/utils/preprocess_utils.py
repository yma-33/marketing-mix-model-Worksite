from mmpac import *
import os


def get_vertica_data():
    # Connect to vertica
    vertica_setup(
        server='aws_qa',  # Or "mm_prod" or "aws_prod"
        user=os.environ['mmm_user'],
        password=os.environ['mmm_pass'])

    sql_fp = "../modeling/sql/"
    impression_table = get_query(sql=read_sql(
        sql_fp + "multi_level_impressions.sql"))
    conversion_table = get_query(sql=read_sql(
        sql_fp + "multi_level_conversions_tera.sql"))
    factor_table = get_query(sql=read_sql(
        sql_fp + "external_factors.sql"))

    # Close connection to save resources after everything done
    vertica_disconnect()
    return impression_table, conversion_table, factor_table
