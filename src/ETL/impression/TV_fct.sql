------------------------------------------------------------
-- TV FACT
DROP TABLE IF EXISTS mix_media_marketing.TV_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.TV_fct AS
SELECT  dma_foo.week,
		dma_foo.channel,
		dma_foo.ad_copy AS campaign,
		dma_factor.STATE as state,
		'Brand Only' AS product,
		sum(dma_foo.impressions) AS initial_impressions,
		sum(dma_foo.impressions * dma_factor.d2s_factor) AS impressions,
		0 AS spend
FROM(
	SELECT *
	FROM mix_media_marketing.Comscore_TV_STG cts
	LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS mmm_csv
	ON UPPER(REGEXP_REPLACE(market,'[.!&-?(\\)\\,\\ ]','')) = mmm_csv.dma_name_raw
) as dma_foo
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON dma_foo.dma_code = dma_factor.dma_code
WHERE dma_foo.week BETWEEN '2017-10-01' AND '2021-06-30' ---need change every quarter!!!!
GROUP BY 1,2,3,4,5
