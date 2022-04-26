-- SELECT TO_DATE(week,'YYYY-IW') as Week, 
-- channel,
-- product,
-- state,
-- region, 
-- campaign_rollup as campaign,
-- impressions
-- FROM mix_media_marketing.impression_campaign
-- WHERE week != '2017-39' 
SELECT week as Week, channel, campaign, mm.state, region, impressions
FROM mix_media_marketing.impression_spend as mm
INNER JOIN mix_media_marketing.csv_region_state as rs ON mm.state = rs.state 
WHERE week != '2017-09-25'
AND mm.state NOT IN ('VI', 'GU', 'AS')