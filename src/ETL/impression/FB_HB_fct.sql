DROP TABLE IF EXISTS mix_media_marketing.FB_HB_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.FB_HB_fct AS
WITH tmp AS (
SELECT a.*, b.dma_code as dma_code_new
FROM (SELECT * FROM mix_media_marketing.GiantSpoon_Social_STG
WHERE dma != 'Unknown') a
LEFT JOIN (SELECT DISTINCT dma_name_raw, dma_code
			FROM mix_media_marketing.csv_dma_code_mapping
			WHERE dma_code != 0) b
ON upper(replace(a.dma, '-', '')) = b.dma_name_raw
)
SELECT week::date AS week, 'Facebook' as channel, campaign, prod_biz_unit AS product,
	dma_factor.STATE as state, ROW_PROCESS_DTM, dma_code_new,
	sum(impressions * 1000) AS initial_impressions,
	sum(impressions * d2s_factor * 1000) AS impressions,
	sum(COALESCE(spend * 1000::float, 0) * d2s_factor) AS spend
FROM tmp a
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON a.dma_code_new = dma_factor.dma_code
GROUP BY 1,2,3,4,5,6,7
