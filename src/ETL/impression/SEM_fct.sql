------------------------------------------------------------
-- SEM FACT
DROP TABLE IF EXISTS mix_media_marketing.SEM_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.SEM_fct AS
SELECT date::date AS week,
  'SEM' AS channel,
  dma_foo.campaign,
  COALESCE(c2p.product, '') AS product,
  dma_factor.STATE as state,
  sum(impressions) AS initial_impressions,
  sum(impressions * d2s_factor) AS impressions,
  sum(COALESCE(cost::float, 0) * d2s_factor) AS spend
FROM(
    SELECT *, impr AS impressions
    FROM (
        SELECT *
        FROM mix_media_marketing.RISE_PaidSearch_STG
        WHERE dma_region_id != ''--< 9000000 -- filter out JAPAN
      ) AS foo
      LEFT JOIN (
        SELECT DISTINCT dma_name,
          dma_code
        FROM mix_media_marketing.csv_dma_code_mapping
      ) AS mmm_csv ON SUBSTRING(dma_region_id, 6, 3) = mmm_csv.dma_code
  ) as dma_foo
  LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
  ON dma_foo.dma_code = dma_factor.dma_code
  LEFT JOIN mix_media_marketing.csv_sem_campaign_prod c2p
  ON dma_foo.campaign = c2p.campaign
WHERE state IS NOT NULL
GROUP BY 1,2,3,4,5
