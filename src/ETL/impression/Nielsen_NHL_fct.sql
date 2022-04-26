------------------------------------------------------------
-- Nielsen NHL
DROP TABLE IF EXISTS mix_media_marketing.NHL_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.NHL_fct AS
SELECT date::date AS week,
		'TV' AS channel,
		'Sponsorship' AS campaign,
		'' AS product,
		dma_factor.STATE AS state,
		sum(replace(qi_sponsorship_impressions, ',', '')) AS initial_impressions,
		sum(replace(qi_sponsorship_impressions, ',', '') * d2s_factor) AS impressions,
		0 AS spend
FROM (
SELECT a.date, CASE WHEN a.qi_sponsorship_impressions = '-'
THEN '0' ELSE a.qi_sponsorship_impressions END AS qi_sponsorship_impressions,
b.dma_code
FROM mix_media_marketing.Nielsen_STG a
LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS b
	ON UPPER(REGEXP_REPLACE(a.market,'[.!&-?(\\)\\,\\ ]','')) = b.dma_name_raw
) as dma_foo
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON dma_foo.dma_code = dma_factor.dma_code
GROUP BY 1,2,3,4,5
