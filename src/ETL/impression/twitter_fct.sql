------------------------------------------------------------
-- Twitter FACT
DROP TABLE IF EXISTS mix_media_marketing.Twitter_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.Twitter_fct AS
SELECT DAY::date AS week,
		'Twitter' AS channel,
		campaign_name AS campaign,
		COALESCE(c2p.product, '') AS product,
		dma_factor.STATE as state,
		sum(impressions) AS initial_impressions,
		sum(impressions * d2s_factor) AS impressions,
		sum(COALESCE(spend::float, 0) * d2s_factor) AS spend
FROM (
	SELECT *
	FROM mix_media_marketing.Twitter_STG as twitter
		LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS mmm_csv
		ON UPPER(
			LEFT(
				twitter.served_location,
				POSITION(
					', US' IN twitter.served_location USING CHARACTERS
				) - 1
			)
		) = mmm_csv.dma_name_raw
) as dma_foo
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON dma_foo.dma_code = dma_factor.dma_code
LEFT JOIN mix_media_marketing.csv_social_campaign_prod c2p
ON dma_foo.campaign_name = c2p.campaign
GROUP BY 1,2,3,4,5
