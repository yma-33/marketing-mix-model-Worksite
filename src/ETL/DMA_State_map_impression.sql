-----------------------------------------
-- state to DMA and DMA to state based on impression
DROP TABLE IF EXISTS mix_media_marketing.dma_state_factor
CREATE TABLE IF NOT EXISTS mix_media_marketing.dma_state_factor AS
WITH tmp AS (
SELECT DMA, STATE, sum(CNTYTVHH) AS TV_sum
FROM mix_media_marketing.county_dma
WHERE DMA NOT IN ('CHARLOTTESVILLE', 'HARRISONBURG', 'NORFOLK-PORTSMTH-NEWPT NWS',
'RICHMOND-PETERSBURG', 'ROANOKE-LYNCHBURG', 'WASHINGTON, DC(HAGRSTWN)', 'TRI-CITIES, TN-VA')
GROUP BY 1,2
),
state_sum AS (
SELECT state, sum(TV_sum) AS TV_state_sum
FROM tmp
GROUP BY 1
),
dma_sum AS (
SELECT DMA, sum(TV_sum) AS TV_dma_sum
FROM tmp
GROUP BY 1
)
SELECT a.*, d.code AS dma_code, b. TV_state_sum, c.TV_dma_sum,
a.TV_sum / b.TV_state_sum AS s2d_factor,
a.TV_sum / c.TV_dma_sum AS d2s_factor
FROM tmp a
INNER JOIN state_sum b
ON a.state = b.state
INNER JOIN dma_sum c
ON a.DMA = c.DMA
LEFT JOIN mix_media_marketing.dma_state_factor_code d
ON upper(a.DMA) = upper(d.DMA)
ORDER BY  DMA, state
