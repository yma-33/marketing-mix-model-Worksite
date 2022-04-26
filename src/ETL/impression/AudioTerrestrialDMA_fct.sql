------------------------------------------------------------
-- AudioTerrestrialDMA FACT
DROP TABLE IF EXISTS mix_media_marketing.AudioTerrestrialDMA_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.AudioTerrestrialDMA_fct AS
WITH dma_foo AS
(
	SELECT *
	FROM mix_media_marketing.GiantSpoon_AudioTerrestrialDMA_STG
    LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS mmm_csv
    ON UPPER(REGEXP_REPLACE(dma, '[.!&-?(\\)\\,\\ ]', '')) = mmm_csv.dma_name_raw
    WHERE dma_code != 0
),
tmp AS (
SELECT week::date,
max(ROW_PROCESS_DTM) AS max_time
FROM dma_foo
GROUP BY 1
),
joined AS (
SELECT a.*, b.max_time
FROM dma_foo a
JOIN tmp b
ON a.week = b.week
)
SELECT week::date,
		channel,
		campaign,
		COALESCE(prod_biz_unit, '') AS product,
		dma_factor.STATE as state,
		sum(REGEXP_REPLACE(impressions, ',', '') * 1000) AS initial_impressions,
		sum(REGEXP_REPLACE(impressions, ',', '') * d2s_factor * 1000) AS impressions,
		sum(spend * d2s_factor * 1000) AS spend
FROM (SELECT * FROM joined WHERE ROW_PROCESS_DTM = max_time) a
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON a.dma_code = dma_factor.dma_code
GROUP BY 1,2,3,4,5
