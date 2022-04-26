------------------------------------------------------------------
-- consumer confidence index
DROP TABLE IF EXISTS mix_media_marketing.cci_hist_raw;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.cci_hist_raw
(week varchar, cci float);
COPY mix_media_marketing.cci_hist_raw FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/External/Consumer_confidence_index_2017_2020.csv' PARSER fcsvparser();

SELECT * FROM mix_media_marketing.cci_hist_raw

------------------------------------------------------------------
-- consumer price index
DROP TABLE IF EXISTS mix_media_marketing.cpi_hist_raw;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.cpi_hist_raw
(week varchar, cpi float);
COPY mix_media_marketing.cpi_hist_raw FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/External/CPI_2017-20.csv' PARSER fcsvparser();

SELECT * FROM mix_media_marketing.cpi_hist_raw

------------------------------------------------------------------
-- gas price index
DROP TABLE IF EXISTS mix_media_marketing.gas_hist_raw;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.gas_hist_raw
(week varchar, gas float);
COPY mix_media_marketing.gas_hist_raw FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/External/GAS_2017-20.csv' PARSER fcsvparser();

SELECT * FROM mix_media_marketing.gas_hist_raw
ORDER BY week
------------------------------------------------------------------
-- unemployment rate
DROP TABLE IF EXISTS mix_media_marketing.unrate_hist_raw;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.unrate_hist_raw
(week varchar, unrate float);
COPY mix_media_marketing.unrate_hist_raw FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/External/UNRATE_2017-20.csv' PARSER fcsvparser();

SELECT * FROM mix_media_marketing.unrate_hist_raw
------------------------------------------------------------------
-- brand health metrics
DROP TABLE IF EXISTS mix_media_marketing.brand_hist_raw;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.brand_hist_raw
(year varchar, awareness float, consideration float, affinity float);

INSERT INTO mix_media_marketing.brand_hist_raw
SELECT '2017', 70, 27, 20
UNION
SELECT '2018', 68.5, 27, 20
UNION
SELECT '2019', 73, 27, 21
UNION
SELECT '2020', 70.5, 27, 20
UNION
SELECT '2021', 70.5, 27, 20.2
