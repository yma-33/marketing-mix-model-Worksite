-------------------------------------
-- impression and spend to share
DROP TABLE IF EXISTS mix_media_marketing.impression_spend;
CREATE TABLE IF NOT EXISTS  mix_media_marketing.impression_spend AS
SELECT week, channel, state, campaign, sum(impressions) AS impressions, sum(spend) AS spend
FROM mix_media_marketing.impression_spend_raw
GROUP BY 1,2,3,4
