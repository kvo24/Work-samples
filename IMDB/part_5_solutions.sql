

-- 5a) Histogram of cast size
-- This lists every movie and its cast size:
create view cast_size as (
      select movie_id, count(*) as number_of_actrixes
      from cast_info
      where role_id = 2 or role_id = 1
      group by movie_id
);
-- This counts the number of occurences of every cast size:
create view hist_cast_size_1 as (
      select number_of_actrixes, count(distinct movie_id) as frequency
      from cast_size
      group by number_of_actrixes
      order by number_of_actrixes desc
);


-- 5b)
select * from COMPANY_TYPE;
-- +----+---------------------------+                                              
-- | ID | KIND                      |
-- |----+---------------------------|
-- |  1 | distributors              |
-- |  2 | production companies      |
-- |  3 | special effects companies |
-- |  4 | miscellaneous companies   |
-- +----+---------------------------+

create or replace view top_10_most_productive_companies as (
    select n.NAME as COMPANY_NAME, n.ID as COMPANY_ID, count(distinct m.MOVIE_ID) as MOVIE_COUNT 
    from COMPANY_NAME n join MOVIE_COMPANIES m on n.ID = m.COMPANY_ID 
    group by n.NAME, n.ID
    order by MOVIE_COUNT desc 
    limit 10
);
-- +----------------------------------------+------------+-------------+           
-- | COMPANY_NAME                           | COMPANY_ID | MOVIE_COUNT |
-- |----------------------------------------+------------+-------------|
-- | Columbia Broadcasting System (CBS)     |          6 |       55656 |
-- | National Broadcasting Company (NBC)    |         19 |       42569 |
-- | American Broadcasting Company (ABC)    |        160 |       35583 |
-- | British Broadcasting Corporation (BBC) |         27 |       19969 |
-- | General Film Company                   |      11137 |       12923 |
-- | Warner Home Video                      |         34 |       10467 |
-- | Warner Bros. Television                |         11 |        9957 |
-- | Granada Television                     |        596 |        8644 |
-- | ABS-CBN                                |        166 |        8176 |
-- | 20th Century Fox Television            |        424 |        8022 |
-- +----------------------------------------+------------+-------------+





with M_2010 as (
    select distinct * 
    from (select m.MOVIE_ID, m.COMPANY_ID 
          from MOVIE_COMPANIES m join TITLE t on m.MOVIE_ID = t.ID 
          where t.PRODUCTION_YEAR = 2010 
    )
)
select n.NAME as COMPANY_NAME, count(distinct m.MOVIE_ID) as MOVIE_COUNT 
from COMPANY_NAME n join M_2010 m on n.ID = m.COMPANY_ID 
group by n.NAME 
order by MOVIE_COUNT desc 
limit 10;



with M_2011 as (
    select distinct * 
    from (select m.MOVIE_ID, m.COMPANY_ID 
          from MOVIE_COMPANIES m join TITLE t on m.MOVIE_ID = t.ID 
          where t.PRODUCTION_YEAR = 2011 
    )
)
select count(distinct NAME) from (select n.NAME as COMPANY_NAME, count(distinct m.MOVIE_ID) as MOVIE_COUNT 
from COMPANY_NAME n join M_2011 m on n.ID = m.COMPANY_ID 
where MOVIE_COUNT > 10 
group by n.NAME 
)
where ;
