DROP TABLE IF EXISTS mix_media_marketing.historical_date
CREATE TABLE IF NOT EXISTS mix_media_marketing.historical_date AS
SELECT ISSUE_DT AS date FROM teradata_hist.AGMT_HIST_VW
WHERE issue_dt BETWEEN '2018-01-01' AND '2021-09-30'
GROUP BY 1 ORDER BY 1

INSERT INTO mix_media_marketing.historical_date
SELECT '2020-08-29'::date
UNION
SELECT '2021-05-29'::date
UNION
SELECT '2021-05-31'::date
UNION
SELECT '2021-07-31'::date
