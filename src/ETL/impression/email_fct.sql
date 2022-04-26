------------------------------------------------------------
-- Email FACT
DROP TABLE IF EXISTS mix_media_marketing.Email_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.Email_fct AS
WITH joined AS (
SELECT a.impression_date AS week, 'Email' AS channel, a.campaign,
upper(a.state) AS state,  'Brand' AS product, sum(cnt) AS impressions,
sum(cnt) * 0.006 AS spend
FROM mix_media_marketing.impression_email_final a
GROUP BY 1,2,3,4,5)
SELECT * FROM joined
WHERE product IS NOT NULL AND state IS NOT NULL
