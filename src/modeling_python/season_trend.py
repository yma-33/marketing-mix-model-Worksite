import pandas as pd
from mmpac import *
import os
import numpy as np
from statsmodels.tsa.seasonal import STL
import datetime

start = '2018-01-01'
end = '2021-09-30'

def get_data():
    vertica_setup(server='aws_prod',
                user=os.environ['MIX_MARKETING_USER'],
                password=os.environ['MIX_MARKETING_PASS'],
                host=os.environ.get("MARKETING_HOST_QA"))

                df_10y = get_query(sql='''SELECT * FROM mix_media_marketing.conversion_season_10y_LOB
                         order by week''')
    return df_10y

def season_trend_lob(df, start, end):
    series10 = df['cnt']
    list10 = list(series10)
    series10 = pd.Series(list10, index=pd.date_range('1-1-2012', periods=len(list10), freq='W-MON'), name = 'sales')

    stl10 = STL(series10, seasonal=53)
    res10 = stl10.fit()

    df_season_10 = pd.DataFrame(res10.seasonal, columns=['season'])
    df_season_10.reset_index(level=0, inplace=True)
    df_season_10 = df_season_10.rename({'index': 'week'}, axis=1)

    df_trend_10 = pd.DataFrame(res10.trend, columns=['trend'])
    df_trend_10.reset_index(level=0, inplace=True)
    df_trend_10 = df_trend_10.rename({'index': 'week'}, axis=1)

    df_season_trend = pd.merge(df_season_10, df_trend_10, on='week', how='inner')
    df_season_trend_3y = df_season_trend[(df_season_trend['week'] >= start)
                                     & (df_season_trend['week'] <= end)]
    return df_season_trend_3y

def season_trend_all(df):
    df_final = pd.DataFrame(columns=['week', 'season', 'trend'])
    for lob in df['LOB'].unique():
        df_season_trend_3y = season_trend(df[df['LOB']==lob], start, end)
        df_season_trend_3y['LOB'] = lob
        df_final = pd.concat([df_final, df_season_trend_3y], ignore_index=True)
    return df_final

if __name__ == "__main__":
    df_10y = get_data()
    df_season_trend = season_trend_all(df_10y)
