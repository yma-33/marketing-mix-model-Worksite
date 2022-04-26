------------------------------------------------------------
-- DisplayZip FACT
DROP TABLE IF EXISTS mix_media_marketing.DisplayZip_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.DisplayZip_fct AS
WITH zip_state AS (
SELECT zip, state
FROM mix_media_marketing.csv_zip_state
GROUP BY 1,2
),
joined AS (
SELECT a.week, a.channel, a.campaign, COALESCE(a.prod_biz_unit, '') AS product, b.state,
sum(a.spend) AS spend, sum(a.impressions) AS impressions
FROM mix_media_marketing.GiantSpoon_DisplayZip_STG a
LEFT JOIN zip_state b
ON a.zip = b.zip
GROUP BY 1,2,3,4,5
)
SELECT * FROM joined
WHERE state IS NOT NULL 
