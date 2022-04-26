SELECT year(week) || '-Q' || quarter(week) AS year_quarter, comb, 
			sum(spend) AS quarterly_spend--year, quarter, comb, count(*) as CPI 
FROM ( -- Query to determine TV CPI by quarter
	(SELECT mmm.week::date, mmm.ch_camp as comb, 
			round(sum(spend),2) as spend, 
			sum(impressions) as impressions
	FROM (
		SELECT week, 
				channel || '_' || campaign as ch_camp, 
				sum(spend) as spend
		FROM mix_media_marketing.tv_spend_201920
		WHERE week != '2017-09-25'
		GROUP BY 1, 2
		ORDER BY 1) as mmm
	LEFT JOIN (
		SELECT week::date, 
					channel || '_' || campaign as ch_camp, 
					sum(impressions) as impressions
		FROM mix_media_marketing.impression_spend as mm
		WHERE week != '2017-09-25'
		GROUP BY 1, 2) as spd 
	ON mmm.week = spd.week
	GROUP BY 1, 2
	)
	UNION
	-- Query to determine other channels' CPI per quarter
	(SELECT week, channel || '_' || campaign AS comb, 
			round(sum(spend),2) as spen, 
			sum(impressions) as impr
	FROM (
		SELECT week::date,
				channel, state, impressions, campaign, spend 
		FROM mix_media_marketing.impression_spend
		WHERE week != '2017-09-25'
		AND channel != 'TV') AS impr_dat
	GROUP BY 1, 2)) AS cpi_table
GROUP BY 1,2
ORDER BY 1,2