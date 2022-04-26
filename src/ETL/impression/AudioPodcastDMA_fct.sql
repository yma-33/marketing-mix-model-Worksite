------------------------------------------------------------
-- AudioPodcastDMA FACT
DROP TABLE IF EXISTS mix_media_marketing.AudioPodcastDMA_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.AudioPodcastDMA_fct AS
SELECT week::date,
		channel,
		campaign,
		COALESCE(prod_biz_unit, '') AS product,
		dma_factor.STATE AS state,
		sum(impressions * 1000) AS initial_impressions,
		sum(impressions * d2s_factor * 1000) AS impressions,
		sum(spend * d2s_factor * 1000) AS spend
FROM(
	SELECT *
	FROM mix_media_marketing.GiantSpoon_AudioPodcastDMA_STG
	LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS mmm_csv
	ON UPPER(REGEXP_REPLACE(DMA,'[.!&-?(\\)\\,\\ ]','')) = mmm_csv.dma_name_raw
) as dma_foo
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON dma_foo.dma_code = dma_factor.dma_code
GROUP BY 1,2,3,4,5
