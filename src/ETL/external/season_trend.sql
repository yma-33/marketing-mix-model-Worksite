-- seasonality AND trend
DROP TABLE IF EXISTS mix_media_marketing.season_trend_hist_raw;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.season_trend_hist_raw
(week varchar, season float, trend float);
COPY mix_media_marketing.season_trend_hist_raw FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Output/season_trend_hist.csv' PARSER fcsvparser();

DROP TABLE IF EXISTS mix_media_marketing.season_trend_hist;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.season_trend_hist AS
SELECT year_iso(week::date) AS YEAR_ISO, year(week::date) AS YEAR, month(week::date) AS MONTH,
week_iso(week::date) AS week, season, trend
FROM mix_media_marketing.season_trend_hist_raw
