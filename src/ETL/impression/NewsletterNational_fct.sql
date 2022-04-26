------------------------------------------------------------
-- NewsletterNational FACT
DROP TABLE IF EXISTS mix_media_marketing.NewsletterNational_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.NewsletterNational_fct AS
WITH state_frac AS (
SELECT state, population,
population / (SELECT sum(population) FROM mix_media_marketing.state_population) AS fraction
FROM mix_media_marketing.state_population
),
joined AS (
SELECT a.week, a.channel, a.campaign, COALESCE(a.prod_biz_unit, '') AS product, b.state,
sum(a.spend * b.fraction * 1000) AS spend, sum(a.impressions * b.fraction * 1000) AS impressions
FROM mix_media_marketing.GiantSpoon_NewsletterNational_STG a
JOIN state_frac b
ON 1 = 1
GROUP BY 1,2,3,4,5)
SELECT * FROM joined
