------------------------------------------------------------
-- OOHZip FACT
DROP TABLE IF EXISTS mix_media_marketing.OOHZip_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.OOHZip_fct AS
WITH stg AS (
SELECT start_date AS week, 'OOH' AS channel, COALESCE(campaign, 'Unknown') AS campaign,
COALESCE(product_bizunit, '') AS product, state, zipcode,
CASE WHEN partner IN ('Billups', 'Captivate') THEN spend::float
     ELSE  spend * 1000 END AS spend,
CASE WHEN partner = 'Billups' THEN impressions::float
	 ELSE impressions * 1000 END AS impressions
FROM mix_media_marketing.GiantSpoon_OOHZip_STG
),
zip_state AS (
SELECT zip, state
FROM mix_media_marketing.csv_zip_state
GROUP BY 1,2
),
state_join AS (
SELECT a.week, 'OOH' AS channel, COALESCE(a.campaign, 'Unknown') AS campaign,
a.product, b.state,
sum(a.spend) AS spend,
sum(a.impressions) AS impressions
FROM (SELECT * FROM stg WHERE state IS NOT NULL) a
LEFT JOIN mix_media_marketing.state_2letter b
ON upper(a.state) = upper(b.state_name)
GROUP BY 1,2,3,4,5
),
zip_join AS (
SELECT a.week, 'OOH' AS channel, COALESCE(a.campaign, 'Unknown') AS campaign,
a.product, b.state,
sum(a.spend) AS spend,
sum(a.impressions) AS impressions
FROM (SELECT * FROM stg WHERE state IS NULL) a
LEFT JOIN zip_state b
ON a.zipcode = b.zip
GROUP BY 1,2,3,4,5
),
unioned AS (
SELECT * FROM state_join
UNION ALL
(SELECT * FROM zip_join)
)
SELECT week, channel, campaign, product, state, spend, impressions
FROM unioned
