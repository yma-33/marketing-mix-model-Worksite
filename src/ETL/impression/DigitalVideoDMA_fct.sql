------------------------------------------------------------
-- DigitalVideoDMA FACT
DROP TABLE IF EXISTS mix_media_marketing.DigitalVideoDMA_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.DigitalVideoDMA_fct AS
SELECT week::date,
		channel,
		campaign,
		COALESCE(prod_biz_unit, '') AS product,
		final_state as state,
		ROW_PROCESS_DTM::date AS ROW_PROCESS_DTM,
		sum(impressions * 1000) AS initial_impressions,
		sum(impressions * d2s_factor_2 * 1000) AS impressions,
		sum(new_spend * d2s_factor_2 * 1000) AS spend
FROM (
    SELECT *,
        CASE
            WHEN dma_factor.STATE IS NOT NULL THEN dma_factor.STATE
            ELSE dma_foo.state
        END as final_state,
        CASE
            WHEN dma_foo.dma_code IS NULL
            AND dma_foo.state IS NOT NULL THEN 1
            ELSE dma_factor.d2s_factor
        END as d2s_factor_2,
        CASE WHEN spend IN ('$-', '-') THEN 0 ELSE spend::float END AS new_spend
    FROM (SELECT * FROM
            (SELECT week, channel, campaign, DMA, prod_biz_unit, regexp_replace(spend, '\$', '') AS spend, impressions, ROW_PROCESS_DTM
				FROM mix_media_marketing.GiantSpoon_DigitalVideoDMA_STG WHERE impressions != '-'
				) a
                LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS mmm_csv
                ON UPPER(REGEXP_REPLACE(a.DMA, '[.!&-?(\\)\\,\\ ]', '')) = mmm_csv.dma_name_raw
                WHERE dma_code != 0
        ) AS dma_foo
        LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
        ON dma_foo.dma_code = dma_factor.dma_code
        ORDER BY spend
) AS foo
WHERE week IS NOT NULL
GROUP BY 1,2,3,4,5,6
