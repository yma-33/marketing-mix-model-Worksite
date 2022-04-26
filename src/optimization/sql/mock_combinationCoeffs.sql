SELECT output, input, coeff, cc.channel_campaign
FROM mix_media_marketing.optimal_combinationCoeffs as cc
LEFT JOIN mix_media_marketing.stakeholder_bounds as sb
ON cc.channel_campaign = sb.CHANNEL_CAMPAIGN 
WHERE cc.coeff > 0 AND {budget_max_column} > 0 
AND cc.output in ('Life_NY_New','Life_CA_New','Life_NY_Old','Ann_NY_New','Ann_NY_Old')