------------------------------------------------------------
-- consolidate 26 data sources
DROP TABLE IF EXISTS mix_media_marketing.impression_fct
CREATE TABLE IF NOT EXISTS mix_media_marketing.impression_fct AS
WITH tmp AS (
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.Twitter_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.FB_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.Instagram_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.Pinterest_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.Snapchat_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.SEM_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.TV_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.Email_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.Dmail_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.PrintNewspaperZip_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.PrintNewspaperState_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.PrintNewspaperNational_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.PrintNewspaperDMA_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.PrintMagazineState_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.NewsletterNational_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.DisplayZip_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.DisplayNational_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.DisplayDMA_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.DisplayCity_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.DigitalVideoDMA_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.DigitalVideoCity_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.CinemaDMA_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.AudioTerrestrialDMA_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.AudioStreamingDMA_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.AudioPodcastDMA_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.OOHZip_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.FB_HB_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.youtube_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.NHL_fct
UNION ALL
SELECT week, channel, campaign, product, state, impressions, spend
FROM mix_media_marketing.LinkedIn_fct
),
joined AS (
SELECT CONCAT(concat(year_iso(a.week::date), '-'), week_iso(a.week::date)) AS week,
CASE WHEN (a.campaign LIKE '%_GDN%' OR a.campaign = 'MASS_NB_DA_Display_NonBrnd_DA_Exact_Web') THEN 'GDN'
	 WHEN a.channel = 'Digital_Display' THEN 'Display'
	 ELSE  a.channel
     END AS channel,
a.campaign,
CASE WHEN a.product IN ('Brand Only', 'MassMutual', 'MMFA') THEN 'Brand'
ELSE a.product END AS product,
a.state, c.region, round(sum(a.impressions)) AS impressions,
sum(a.spend) AS spend
FROM tmp a
JOIN (SELECT state FROM mix_media_marketing.state_2letter
GROUP BY 1) b
ON a.state = b.state
JOIN mix_media_marketing.csv_region_state c
ON a.state = c.state
WHERE impressions IS NOT NULL
AND product NOT IN ('Workcite', 'Workplace', 'In Good Company', 'Immerse',  'HR Recruitment')
AND week::date BETWEEN '2017-10-01' AND '2021-09-30' -- need to change every quarter
GROUP BY 1,2,3,4,5,6
)
SELECT week, channel, campaign, product, state, region,
CASE WHEN (channel IN ('Digital_Video') AND campaign = 'Holiday_2019')  THEN (impressions / 1000) ELSE impressions END AS impressions,
CASE WHEN (channel IN ('Digital_Video') AND campaign = 'Holiday_2019') THEN (spend / 1000) ELSE spend END AS spend
FROM joined 
