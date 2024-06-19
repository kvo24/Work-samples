-- 1a)
with who_directed_most_movies as (
      select top 1 person_id as director, count(distinct movie_id) as movie_count
      from cast_info
      where role_id = 8
      group by person_id
      order by movie_count desc
)
select n.name as most_prolific_director
from name n, who_directed_most_movies w
where n.id = w.director;


-- 1b)
create or replace view dick_carson_movies as
select distinct movie_id
from cast_info
where person_id = 244652 and role_id = 8;

with all_dick_carson_collaborators as (
      select c.person_id as collaborator, count(*) as number_of_collaborations
      from cast_info c join dick_carson_movies d on c.movie_id = d.movie_id
      where c.person_id != 244652
      group by c.person_id
)
select top 10 n.name as dick_carson_top_10_collaborators
from name n join all_dick_carson_collaborators a on n.id = a.collaborator
order by a.number_of_collaborations desc;


-- 1c)
with dick_carson_career as (
      select count(*) as number_of_movies_directed, to_number(t.production_year) as year
      from title t join dick_carson_movies d on t.id = d.movie_id
      group by year
)
select top 1 year as dick_carson_most_productive_year
from dick_carson_career c
order by c.number_of_movies_directed desc;


-- 2a)
create or replace view budget_table as
with unmaxed_budget as (
      select movie_id, to_number(regexp_substr(replace(regexp_substr(info, '[$].*'), ',', ''), '[0-9]+')) as budget 
      from movie_info 
      where INFO_TYPE_ID = 105 and budget is not null
)
select movie_id, max(budget) as movie_budget
from unmaxed_budget
group by movie_id;


-- 2b)
create or replace view gross_table as
with unmaxed_gross as (
      select movie_id, to_number(regexp_substr(replace(regexp_substr(info, '[$].*'), ',', ''), '[0-9]+')) as gross 
      from movie_info 
      where info_type_id = 107 and gross is not null
)
select movie_id, max(gross) as movie_gross_revenue
from unmaxed_gross
group by movie_id;


-- 2c)
with movie_profits as (
      select t.title as movie_title, g.movie_gross_revenue - b.movie_budget as profit 
      from gross_table g inner join budget_table b on g.movie_id = b.movie_id inner join title t on g.movie_id = t.id 
      where profit is not null 
      order by profit desc 
      limit 10
)
select movie_title as top_10_most_profitable_movies
from movie_profits;


-- 3a)
with directors as (
      select person_id, movie_id
      from cast_info 
      where role_id = 8
),
actrixes as (
      select person_id, movie_id
      from cast_info
      where role_id = 2 or role_id = 1
)
select count(distinct d.movie_id) as number_of_movies_where_director_also_acts
from directors d, actrixes a
where d.person_id = a.person_id and d.movie_id = a.movie_id;


-- 3b)
with cast_size as (
      select movie_id, count(*) as number_of_actrixes
      from cast_info
      where role_id = 2 or role_id = 1
      group by movie_id
)
select avg(number_of_actrixes)
from cast_size;
