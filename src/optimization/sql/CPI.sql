SELECT comb, sum(spend) / sum(impressions) as CPI 
FROM ( -- Query to determine TV CPI by quarter because TV spend is not in impression_spend
		(
			SELECT year_iso(mmm.week) AS year, 
			quarter(mmm.week) AS quarter,
			mmm.channel || '_' || mmm.campaign AS comb,
			round(sum(mmm.spend),2) as spend,
			sum(spd.impressions) as impressions
			FROM mix_media_marketing.tv_spend_201920 AS mmm
			LEFT JOIN  -- Group impressions by channel_campaign to join to tv_spend_201920
				(SELECT week, channel, campaign, sum(impressions) as impressions
				FROM mix_media_marketing.impression_spend
				GROUP BY 1,2,3) AS spd
			ON mmm.week::date = spd.week::date 
			AND mmm.channel = spd.channel
			AND mmm.campaign = spd.campaign
			GROUP BY 1, 2, 3
		) 
	UNION
		-- Query to determine other channels' CPI per quarter
		(
			SELECT year, quarter, channel || '_' || campaign AS comb, 
				round(sum(spend),2) as spen, 
				sum(impressions) as impr
			FROM (
				SELECT year_iso(week::date) as year,
						quarter(week::date) as quarter, 
						channel, state, impressions, campaign, spend 
				FROM mix_media_marketing.impression_spend
				WHERE week != '2017-09-25'  -- Only one week in that quarter
				AND channel != 'TV') AS impr_dat
			GROUP BY 1, 2, 3
		)
) AS cpi_table
WHERE year IN {sql_year}
AND quarter IN {sql_quarter}
GROUP BY comb 