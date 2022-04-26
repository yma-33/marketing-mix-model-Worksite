------------------------------------------------------------------------
-- Direct Mail
DROP TABLE IF EXISTS mix_media_marketing.impression_direct_mail;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.impression_direct_mail AS
SELECT FACT.FACT_PARTY_EVENT_NATURAL_KEY_HASH_UUID,
FACT.EVENT_DTM::DATE AS impression_date, DC.campaign_id, DC.campaign_nm, campaign_line_of_business_cde AS lob,
ATTR.state_cde
FROM EDW_CIP_VW.FACT_PARTY_EVENT_VW FACT
JOIN EDW_CIP_VW.PARTY_MASTER_OF_MASTERS_XREF_VW XREF
	ON FACT.DIM_PARTY_NATURAL_KEY_HASH_UUID = XREF.DIM_PRIOR_PARTY_NATURAL_KEY_HASH_UUID
JOIN EDW_CIP_VW.DIM_PARTY_VW DP
	ON DP.DIM_PARTY_NATURAL_KEY_HASH_UUID = XREF.DIM_PARTY_NATURAL_KEY_HASH_UUID
JOIN EDW_CIP_VW.REF_EVENT_SOURCE_VW RES
	ON FACT.REF_EVENT_SOURCE_NATURAL_KEY_HASH_UUID = RES.REF_EVENT_SOURCE_NATURAL_KEY_HASH_UUID
JOIN EDW_CIP_VW.REF_EVENT_TYPE_VW RET
	ON FACT.REF_EVENT_TYPE_NATURAL_KEY_HASH_UUID = RET.REF_EVENT_TYPE_NATURAL_KEY_HASH_UUID
JOIN EDW_CIP_VW.ATTR_DIRECT_MAIL_COMMUNICATION_EVENT_VW ATTR
	ON FACT.FACT_PARTY_EVENT_NATURAL_KEY_HASH_UUID = ATTR.FACT_PARTY_EVENT_NATURAL_KEY_HASH_UUID
LEFT OUTER JOIN (SELECT * FROM EDW_CIP_VW.DIM_CAMPAIGN_VW WHERE campaign_line_of_business_cde = 'Mmfa') DC
	ON ATTR.DIM_CAMPAIGN_NATURAL_KEY_HASH_UUID = DC.DIM_CAMPAIGN_NATURAL_KEY_HASH_UUID
WHERE
FACT.CURRENT_ROW_IND = TRUE AND FACT.LOGICAL_DELETE_IND = FALSE AND FACT.EVENT_DTM::DATE BETWEEN '2017-10-01' AND '2020-12-31'
AND XREF.CURRENT_ROW_IND = TRUE AND XREF.LOGICAL_DELETE_IND = FALSE AND XREF.PARTY_ID_TYPE_CDE = 'Mstr_prty_id'
AND DP.CURRENT_ROW_IND = TRUE AND DP.LOGICAL_DELETE_IND = FALSE
AND RES.CURRENT_ROW_IND = TRUE AND RES.LOGICAL_DELETE_IND = FALSE AND RES.EVENT_SOURCE_CDE = 'Sfmc'
AND RET.CURRENT_ROW_IND = TRUE AND RET.LOGICAL_DELETE_IND = FALSE AND RET.EVENT_GROUP_CDE = 'Mktcommn' AND RET.EVENT_CATEGORY_CDE = 'Dmcommn' AND RET.EVENT_TYPE_CDE = 'Send'
AND ATTR.CURRENT_ROW_IND = TRUE AND ATTR.LOGICAL_DELETE_IND = FALSE
AND DC.CURRENT_ROW_IND = TRUE AND DC.LOGICAL_DELETE_IND = FALSE
AND ATTR.state_cde IS NOT NULL
GROUP BY 1,2,3,4,5,6

------------------------------------------------------------------
-- SF as source
DROP TABLE IF EXISTS mix_media_marketing.impression_email_dmail;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.impression_email_dmail AS
WITH joined AS (
SELECT b.Id, b.Name AS Campaign_NM, b.Types, a.Status, a.HasResponded,
a.CreatedDate::date AS Date, a.MM_Best_Known_State AS state
FROM esp_leads.SF_MM_CAMPAIGN_MEMBER a -- each record is an event
LEFT JOIN (SELECT * FROM esp_leads.SF_MM_CAMPAIGN
where Types IN ('Email', 'Direct Mail')) b
ON a.CampaignId = b.Id
)
SELECT * FROM joined
WHERE date BETWEEN '2017-10-01' AND '2020-12-31'

------------------------------------------------------------------
-- SF and SFMC consolidation, direct mail
DROP TABLE IF EXISTS mix_media_marketing.impression_dmail_final;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.impression_dmail_final AS
WITH sf AS (
SELECT Id, Campaign_NM AS campaign, date AS impression_date, state, count(*) AS cnt
FROM mix_media_marketing.impression_email_dmail
WHERE Types = 'Direct Mail'
GROUP BY 1,2,3,4
) ,
sfmc AS (
SELECT campaign_id AS Id, campaign_nm AS campaign, impression_date, state_cde AS state, count(*) AS cnt
FROM mix_media_marketing.impression_direct_mail
GROUP BY 1,2,3,4
),
joined AS (
SELECT COALESCE(b.Id, a.Id) AS Id, COALESCE(b.campaign, a.campaign) AS campaign,
COALESCE(b.impression_date, a.impression_date) AS impression_date,
COALESCE(b.state, a.state) AS state, COALESCE(b.cnt, a.cnt) AS cnt,
a.Id AS sf_id, b.id AS sfmc_id
FROM sf a
FULL OUTER JOIN sfmc b
ON upper(a.Id) = upper(b.Id)
AND a.impression_date = b.impression_date
AND upper(a.state) = upper(b.state)
)
SELECT * FROM joined
