SELECT
    year_iso(week::date) || '-' || quarter(week::date) AS year_quarter,
    comb,
    sum(spend) AS spend,
    sum(impressions) AS impressions
FROM
    (
        -- Query to determine TV CPI by quarter
        (
            SELECT
                mmm.week,
                mmm.ch_camp AS comb,
                round(sum(spend), 2) AS spend,
                sum(impressions) AS impressions
            FROM
                (
                    SELECT
                        week :: date,
                        channel || '_' || campaign AS ch_camp,
                        sum(spend) AS spend
                    FROM
                        mix_media_marketing.tv_spend_201920
                    WHERE
                        week != '2017-09-25'
                    GROUP BY
                        1,
                        2
                    ORDER BY
                        1
                ) AS mmm
                LEFT JOIN (
                    SELECT
                        week :: date,
                        channel || '_' || campaign AS ch_camp,
                        sum(impressions) AS impressions
                    FROM
                        mix_media_marketing.impression_spend AS mm
                    WHERE
                        week != '2017-09-25'
                    GROUP BY
                        1,
                        2
                ) AS spd ON mmm.week = spd.week
                AND mmm.ch_camp = spd.ch_camp
            GROUP BY
                1,
                2
        )
        UNION
        -- Query to determine other channels' CPI per quarter
        (
            SELECT
                week :: date,
                channel || '_' || campaign AS comb,
                round(sum(spend), 2) AS spen,
                sum(impressions) AS impr
            FROM
                (
                    SELECT
                        week :: date,
                        channel,
                        state,
                        impressions,
                        campaign,
                        spend
                    FROM
                        mix_media_marketing.impression_spend
                    WHERE
                        week != '2017-09-25'
                        AND channel != 'TV'
                ) AS impr_dat
            GROUP BY
                1,
                2
        )
    ) AS cpi_table
GROUP BY
    1,
    2