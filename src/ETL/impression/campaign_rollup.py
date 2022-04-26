import pandas as pd
from os import listdir
from mmpac import *
import os
from mmpac import create_table
from mmpac import bulk_load
import numpy as np

vertica_setup(server='aws_qa',
             user=os.environ['MIX_MARKETING_USER'],
             password=os.environ['MIX_MARKETING_PASS'],
             host=os.environ.get("MARKETING_HOST_QA"))

df_imp = get_query(sql='''SELECT TO_DATE(week,'YYYY-IW') as week, channel, campaign, state, impressions, spend
                        FROM mix_media_marketing.impression_fct''')
print(df_imp.shape)
# replace MMxxxxx with user MM ID
path = '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/MMFA/campaign/channel_campaign_mapping_20211105.csv'
df_map = pd.read_csv(path)
df_join = pd.merge(df_imp, df_map, on=['campaign', 'channel'], how='left', suffixes=("_x", "_y"))
df_filter = df_join[(df_join['channel'] != 'TV') | \
                    ((df_join['channel'] == 'TV') & (df_join['Campaign Roll Up '] != 'Unknown'))]
df_upload = df_filter[['week', 'channel',  'state', 'impressions', 'Campaign Roll Up ', 'spend']]
print(df_upload.shape)

# check if there is campaign in impression table that cannot be mapped using map dic
df_sub = df_upload[df_upload['Campaign Roll Up '].isna()]
df_sub_na = df_sub.groupby(['channel','campaign']).size().reset_index().rename(columns={0:'count'})

bulk_load(df_upload, 'mix_media_marketing', 'impression_spend_raw')
