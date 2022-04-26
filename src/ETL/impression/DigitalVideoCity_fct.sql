------------------------------------------------------------
-- DigitalVideoCity FACT
DROP TABLE IF EXISTS mix_media_marketing.DigitalVideoCity_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.DigitalVideoCity_fct AS
WITH tmp AS (
SELECT week::date, channel, campaign,  COALESCE(prod_biz_unit, '') AS product,
RIGHT(REGEXP_SUBSTR(dma, '-[A-Z]{2}$'), 2) AS state,
sum(spend * 1000) AS spend, sum(impressions * 1000) AS impressions
FROM mix_media_marketing.GiantSpoon_DigitalVideoCity_STG
WHERE dma NOT IN ('N/A-N/A')
GROUP BY 1,2,3,4,5
)
SELECT * FROM tmp
