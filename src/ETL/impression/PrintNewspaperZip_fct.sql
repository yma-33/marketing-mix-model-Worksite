------------------------------------------------------------
-- PrintNewspaperZip FACT
DROP TABLE IF EXISTS mix_media_marketing.PrintNewspaperZip_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.PrintNewspaperZip_fct AS
SELECT week, channel, campaign, 'MA' AS state, COALESCE(prod_biz_unit, '') AS product,
impressions * 1000 AS impressions, spend * 1000 AS spend
FROM mix_media_marketing.GiantSpoon_PrintNewspaperZip_STG 
