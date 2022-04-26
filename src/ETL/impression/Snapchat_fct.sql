------------------------------------------------------------
-- Snapchat FACT
DROP TABLE IF EXISTS mix_media_marketing.Snapchat_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.Snapchat_fct AS
WITH dma_foo AS
(
		SELECT *,
			totalimpression AS impressions,
			mmm_csv.dma_code as dma_code_true
		FROM mix_media_marketing.Snapchat_STG as snap
			LEFT JOIN (
				SELECT DISTINCT dma_name,
					dma_code
				FROM mix_media_marketing.csv_dma_code_mapping
			) AS mmm_csv ON SUBSTRING(
				snap.DMA_CODE,
				POSITION('/' IN snap.DMA_CODE USING CHARACTERS) + 1,
				LENGTH(snap.DMA_CODE) - POSITION('/' IN snap.DMA_CODE USING CHARACTERS) - 1
			) = mmm_csv.dma_code
	),
tmp AS (
SELECT week_start_mon::date,
max(ROW_PROCESS_DTM) AS max_time
FROM dma_foo
GROUP BY 1
),
joined AS (
SELECT a.*, b.max_time
FROM dma_foo a
JOIN tmp b
ON a.week_start_mon = b.week_start_mon
)
SELECT week_start_mon::date AS week,
	'Snapchat' AS channel,
	campaignname AS campaign,
	COALESCE(c2p.product, '') AS product,
	dma_factor.STATE as state,
	ROW_PROCESS_DTM,
	sum(impressions) AS initial_impressions,
	sum(impressions * d2s_factor) AS impressions,
	sum(COALESCE(spend::float, 0) * d2s_factor) AS spend
FROM (SELECT * FROM joined WHERE ROW_PROCESS_DTM = max_time) a
	LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
	ON a.dma_code_true = dma_factor.dma_code
	LEFT JOIN mix_media_marketing.csv_social_campaign_prod c2p
	ON a.campaignname = c2p.campaign
	GROUP BY 1,2,3,4,5,6
