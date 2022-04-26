------------------------------------------------------------
-- Instagram FACT
DROP TABLE IF EXISTS mix_media_marketing.Instagram_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.Instagram_fct AS
WITH tmp AS (
SELECT date,
max(FILE_LOAD_TIME) AS max_time
FROM mix_media_marketing.facebook_STG
GROUP BY 1
),
joined AS (
SELECT a.*, b.max_time
FROM mix_media_marketing.facebook_STG a
JOIN tmp b
ON a.date = b.date
),
tmp2 AS (
SELECT a.date AS week, a.platform AS channel, a.campaign_name AS campaign,
COALESCE(c2p.product, '') AS product, b.state AS state, a.impressions, a.spend
FROM (SELECT * FROM joined WHERE FILE_LOAD_TIME = max_time
AND platform = 'Instagram') a
LEFT JOIN mix_media_marketing.state_2letter b
ON upper(a.region) = upper(b.state_name)
LEFT JOIN mix_media_marketing.csv_social_campaign_prod c2p
ON a.campaign_name = c2p.campaign
)
SELECT week, channel, campaign, product, state, sum(impressions) AS impressions,
sum(COALESCE(spend::float, 0)) AS spend
FROM tmp2 WHERE state IS NOT NULL
GROUP BY 1,2,3,4,5 
