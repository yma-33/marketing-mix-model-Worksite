------------------------------------------------------------
-- Pinterest FACT
DROP TABLE IF EXISTS mix_media_marketing.Pinterest_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.Pinterest_fct AS
SELECT date::date AS week,
		'Pinterest' AS channel,
		campaign_name AS campaign,
		COALESCE(c2p.product, '') AS product,
		dma_factor.STATE as state,
		sum(impressions) AS initial_impressions,
		sum(impressions * d2s_factor) AS impressions,
		sum(COALESCE(spend_in_account_currency::float, 0) * d2s_factor) AS spend
FROM(
	SELECT *
	FROM mix_media_marketing.Pinterest_STG as pin
		LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS mmm_csv
		ON UPPER(
			SUBSTRING(
				pin.targeting_value,
				LENGTH('U.S.: ') + 1,
				LENGTH(pin.targeting_value)
			)
		) = mmm_csv.dma_name_raw
) as dma_foo
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON dma_foo.dma_code = dma_factor.dma_code
LEFT JOIN mix_media_marketing.csv_social_campaign_prod c2p
ON dma_foo.campaign_name = c2p.campaign
GROUP BY 1,2,3,4,5
