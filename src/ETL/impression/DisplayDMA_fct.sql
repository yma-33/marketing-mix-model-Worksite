------------------------------------------------------------
-- DisplayDMA FACT
DROP TABLE IF EXISTS mix_media_marketing.DisplayDMA_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.DisplayDMA_fct AS
SELECT week::date,
    channel,
    campaign,
    COALESCE(prod_biz_unit, '') AS product,
    final_state AS state,
    original_DMA,
    sum(impressions * 1000) AS initial_impressions,
	sum(impressions * d2s_factor_2 * 1000) AS impressions,
	sum(spend * d2s_factor_2 * 1000) AS spend
	FROM (
        SELECT dma_foo.*,
        	dma_foo.DMA as original_DMA,
            CASE
                WHEN dma_factor.STATE IS NOT NULL THEN dma_factor.STATE
                ELSE dma_foo.state
            END as final_state,
            CASE
                WHEN dma_foo.dma_code IS NULL
                AND dma_foo.state IS NOT NULL THEN 1
                ELSE dma_factor.d2s_factor
            END as d2s_factor_2
    	 FROM (
        	SELECT *,
				-- Extract state when in city, state format i.e."BOSTON-MA" -->"MA"
				-- Only if DMA code is not mapped
				CASE
					WHEN dma_code IS NULL THEN RIGHT(REGEXP_SUBSTR(DMA, '-[A-Z]{2}$'), 2)
					ELSE NULL
				END AS state
			FROM (
				(SELECT week, channel, campaign, DMA, prod_biz_unit, regexp_replace(spend, '\$', '') AS spend, impressions
				FROM mix_media_marketing.GiantSpoon_DisplayDMA_STG
				WHERE DMA IS NOT NULL AND DMA != 'none' AND impressions != '-') a
				LEFT JOIN (
					SELECT dma_name_raw, dma_code
					FROM mix_media_marketing.csv_dma_code_mapping) AS mmm_csv
				ON UPPER(REGEXP_REPLACE(a.DMA, '[.!&-?(\\)\\,\\ ]', '')) = mmm_csv.dma_name_raw
			)   AS foo WHERE dma_code != 0
        )  AS dma_foo
        LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
        ON dma_foo.dma_code = dma_factor.dma_code
 ) AS baa
 GROUP BY 1,2,3,4,5,6
