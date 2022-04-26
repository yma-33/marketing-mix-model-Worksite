------------------------------------------------------------------
-- active customers volumes, conversion data is prerequisite
DROP TABLE IF EXISTS mix_media_marketing.tera_conversion;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.tera_conversion AS
SELECT policy_number, AGREEMENT_SOURCE_CD, state
FROM mix_media_marketing.conversion_tera_all_seg_mmfa
GROUP BY 1,2,3

DROP TABLE IF EXISTS mix_media_marketing.total_agg_stg1;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.total_agg_stg1 AS
WITH tmp1 AS (
SELECT TRIM(a.HLDG_KEY) as policy_number, a.AGREEMENT_SOURCE_CD, b.date AS agg_date,
CASE WHEN b.date BETWEEN a.AGMT_HIST_FR_DT AND a.AGMT_HIST_TO_DT THEN 1
ELSE 0 END AS cnt
FROM (SELECT * FROM teradata_hist.AGMT_HIST_VW
WHERE HLDG_STUS = 'IF') a
JOIN (SELECT * FROM mix_media_marketing.historical_date
WHERE date <= '2019-10-31') b -- split to address long time run issue
ON 1 = 1
),
geo AS (
SELECT a.policy_number, a.AGREEMENT_SOURCE_CD, b.state, a.agg_date, a.cnt
FROM tmp1 a
JOIN mix_media_marketing.tera_conversion b
ON a.policy_number = b.policy_number
AND a.AGREEMENT_SOURCE_CD = b.AGREEMENT_SOURCE_CD
)
SELECT state, agg_date, sum(cnt) AS total_cnt
FROM geo
GROUP BY 1,2

DROP TABLE IF EXISTS mix_media_marketing.total_agg_stg2;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.total_agg_stg2 AS
WITH tmp1 AS (
SELECT TRIM(a.HLDG_KEY) as policy_number, a.AGREEMENT_SOURCE_CD, b.date AS agg_date,
CASE WHEN b.date BETWEEN a.AGMT_HIST_FR_DT AND a.AGMT_HIST_TO_DT THEN 1
ELSE 0 END AS cnt
FROM (SELECT * FROM teradata_hist.AGMT_HIST_VW
WHERE HLDG_STUS = 'IF') a
JOIN (SELECT * FROM mix_media_marketing.historical_date
WHERE date > '2019-10-31') b -- split to address long time run issue
ON 1 = 1
),
geo AS (
SELECT a.policy_number, a.AGREEMENT_SOURCE_CD, b.state, a.agg_date, a.cnt
FROM tmp1 a
JOIN mix_media_marketing.tera_conversion b
ON a.policy_number = b.policy_number
AND a.AGREEMENT_SOURCE_CD = b.AGREEMENT_SOURCE_CD
)
SELECT state, agg_date, sum(cnt) AS total_cnt
FROM geo
GROUP BY 1,2

DROP TABLE IF EXISTS mix_media_marketing.customer_volume;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.customer_volume AS
WITH total AS (
SELECT state, agg_date, total_cnt
FROM mix_media_marketing.total_agg_stg1
UNION ALL
(SELECT state, agg_date, total_cnt
FROM mix_media_marketing.total_agg_stg2)
),
agg AS (
SELECT state, TO_DATE(CONCAT(concat(year_iso(agg_date::date), '-'), week_iso(agg_date::date)), 'YYYY-IW') AS week,
sum(total_cnt) AS total_cnt
FROM total
GROUP BY 1,2
)
SELECT a.state, year_iso(a.week) AS year_iso, week_iso(a.week) AS week_iso, year(a.week) AS YEAR, month(a.week) AS MONTH,
CONCAT(concat(year_iso(week), '-'), week_iso(week)) AS new_week, a.total_cnt
FROM agg a
JOIN (SELECT state FROM mix_media_marketing.state_2letter
GROUP BY 1) b
ON a.state = b.state
WHERE a.week != '2017-09-25'
