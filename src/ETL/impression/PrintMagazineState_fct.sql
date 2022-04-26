------------------------------------------------------------
-- PrintMagazineState FACT
DROP TABLE IF EXISTS mix_media_marketing.PrintMagazineState_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.PrintMagazineState_fct AS
SELECT week, channel, campaign, dma AS state, COALESCE(prod_biz_unit, '') AS product,
sum(impressions * 1000) AS impressions, sum(spend * 1000) AS spend
FROM mix_media_marketing.GiantSpoon_PrintMagazineState_STG
GROUP BY 1,2,3,4,5
