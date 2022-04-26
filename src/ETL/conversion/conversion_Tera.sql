DROP TABLE IF EXISTS mix_media_marketing.conversion_tera_all_mmfa_test;
CREATE TABLE  IF NOT EXISTS  mix_media_marketing.conversion_tera_all_mmfa_test AS
WITH remove AS (
SELECT t.AGREEMENT_ID
FROM teradata.CUST_AGMT_CMN_VW t
LEFT JOIN teradata.CUST_DEMOGRAPHICS_VW c -- use prod_usig_stnd_vw when it's available
ON t.prty_id = c.prty_id
WHERE PRTY_TYP_CD='N'
AND SNS_PRTY_IND='Y'
GROUP BY 1
),
filtered AS (
SELECT a.*, b.AGREEMENT_ID AS remove_AGREEMENT_ID
FROM teradata.AGMT_CMN_VW a
LEFT JOIN remove b
ON a.AGREEMENT_ID = b.AGREEMENT_ID
),
effect_date AS (
SELECT TRIM(HLDG_KEY) as policy_number, AGREEMENT_SOURCE_CD,
-- for joining with SIERA
--HLDG_KEY_PFX, HLDG_KEY_SFX,
CASE WHEN (AGREEMENT_SOURCE_CD IN ('HAVEN', 'HAP') OR LOB_CDE IN ('ACCIDENT', 'CRIT ILL', 'IMG     ', 'INVS    ', 'LCM     ', 'LENDING ', 'RETIRE  '))
THEN POLICY_EFF_DATE ELSE ISSUE_DT END AS ISSUE_DT,
MAJOR_PROD_NME as major_product, MINOR_PROD_CDE AS minor_product, LOB_CDE AS LOB, PROD_ID,
GREATEST(
  case when TOT_ANNL_PREM_AMT is NULL then 0 else TOT_ANNL_PREM_AMT end,
  case when AGMT_TOT_ANNUAL_PREM_AMT is NULL then 0 else AGMT_TOT_ANNUAL_PREM_AMT end,
  case when MODAL_PREM_ANNL is NULL then 0 else MODAL_PREM_ANNL end)::!float as tot_premium
FROM (SELECT * FROM filtered WHERE remove_AGREEMENT_ID IS NULL AND PROD_TYP_CDE != 'GALI'
	) a
WHERE HLDG_STUS in ('IF', 'TM') AND trim(stus_rsn) != 'NT'
AND trim(MAJOR_PROD_NME) != '' --AND ISSUE_DT > '2010-01-01'
AND AGREEMENT_SOURCE_CD NOT IN ('AFAS', 'FDPB00', 'FDPX00', 'WRKSTE')
AND WORKSITE_IND != 'Y'
GROUP BY 1,2,3,4,5,6,7,8
),
rnk AS (
select *,
ROW_NUMBER() OVER(PARTITION BY policy_number, AGREEMENT_SOURCE_CD ORDER BY ISSUE_DT) AS policy_rnk
FROM effect_date
),
party AS (
SELECT PRTY_ID, TRIM(HLDG_KEY) as policy_number, AGREEMENT_SOURCE_CD
FROM teradata.CUST_AGMT_CMN_VW
WHERE PRTY_AGMT_RLE_CD = 'OWNR'
GROUP BY 1,2,3
),
demo AS (
SELECT PRTY_ID as owner_id, state
FROM teradata.CUST_DEMOGRAPHICS_VW
WHERE PRTY_TYP_CD = 'I' and
GOVT_ID_NR is not NULL and
BRTH_DT > '1900-01-01'
GROUP BY 1,2
),
joined AS (-- a policy can have multiple owners and they can be in different states, may need to figure out later
SELECT a.*, c.state, b.PRTY_ID
FROM (SELECT * FROM rnk WHERE policy_rnk = 1 AND ISSUE_DT > '2010-01-01') a
INNER JOIN party b
ON a.policy_number = b.policy_number
AND trim(a.AGREEMENT_SOURCE_CD) = trim(b.AGREEMENT_SOURCE_CD)
INNER JOIN demo c
ON b.PRTY_ID = c.owner_id
)
SELECT a.policy_number, a.AGREEMENT_SOURCE_CD, --a.HLDG_KEY_PFX, a.HLDG_KEY_SFX,
a.ISSUE_DT, a.major_product, a.minor_product, a.LOB, a.PROD_ID, a.tot_premium, a.state, c.region
FROM joined a
JOIN (SELECT state FROM mix_media_marketing.state_2letter
GROUP BY 1) b
ON upper(a.state) = b.state
JOIN mix_media_marketing.csv_region_state c
ON upper(a.state) = c.state
GROUP BY 1,2,3,4,5,6,7,8,9,10;
