------------------------------------------------------------
-- PrintNewspaperDMA FACT
DROP TABLE IF EXISTS mix_media_marketing.PrintNewspaperDMA_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.PrintNewspaperDMA_fct AS
SELECT week,
		channel,
		campaign,
		COALESCE(prod_biz_unit, '') AS product,
		dma_factor.STATE as state,
		sum(impressions * 1000) AS initial_impressions,
		sum(impressions * d2s_factor * 1000) AS impressions,
		sum(spend * d2s_factor * 1000) AS spend
FROM (
	SELECT *
	FROM mix_media_marketing.GiantSpoon_PrintNewspaperDMA_STG as gsp
		LEFT JOIN (
			SELECT DISTINCT dma_code,
				REPLACE (dma_name_raw, ' ', '') as dma_name
			FROM mix_media_marketing.csv_dma_code_mapping AS mmm_csv
		) as mm_dma ON UPPER(gsp.dma) = mm_dma.dma_name
		WHERE dma_code != 0
) AS newspaper_dma
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON newspaper_dma.dma_code = dma_factor.dma_code
GROUP BY 1,2,3,4,5
