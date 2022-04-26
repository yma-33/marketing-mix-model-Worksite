DROP TABLE IF EXISTS mix_media_marketing.youtube_raw;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.youtube_raw
(week varchar, campaign varchar, geo varchar, spend float, impressions float);
COPY mix_media_marketing.youtube_raw FROM LOCAL '/Users/MMxxxxx/Documents/Data Science/Projects/MMM/Data source/MMFA/Youtube/MMM_YouTube_all time.csv' PARSER fcsvparser()


DROP TABLE IF EXISTS mix_media_marketing.youtube_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.youtube_fct AS
SELECT week::date,
		'YouTube' AS channel,
		campaign,
		'' AS product,
		dma_factor.STATE AS state,
		sum(impressions) AS initial_impressions,
		sum(impressions * d2s_factor) AS impressions,
		sum(spend * d2s_factor) AS spend
FROM(
	SELECT *
	FROM mix_media_marketing.youtube_raw a
	LEFT JOIN mix_media_marketing.csv_dma_code_mapping AS b
	ON UPPER(REGEXP_REPLACE(a.geo,'[.!&-?(\\)\\,\\ ]','')) = b.dma_name_raw
) as dma_foo
LEFT JOIN mix_media_marketing.dma_state_factor AS dma_factor
ON dma_foo.dma_code = dma_factor.dma_code
GROUP BY 1,2,3,4,5
