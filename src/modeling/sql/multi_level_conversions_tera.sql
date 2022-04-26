SELECT * FROM (
	SELECT 
	TO_DATE(CONCAT(concat(year_iso(ISSUE_DT::date), '-'), week_iso(ISSUE_DT::date)), 'YYYY-IW') AS Week, 
	state, INITCAP(LOB) AS LOB, INITCAP(major_product) AS product, region, segment, sum(tot_premium) as total_premium,
	count(DISTINCT policy_number || '_' || AGREEMENT_SOURCE_CD) AS conversion
	FROM mix_media_marketing.conversion_tera_all_seg_mmfa
	WHERE ISSUE_DT BETWEEN '2018-07-01' AND '2021-09-30'
	AND major_product NOT IN ('')
	AND state NOT IN ('VI', 'GU', 'AS')
	GROUP BY Week, state, LOB, major_product, region, segment) AS foo 
WHERE segment IS NOT NULL
ORDER BY Week;
