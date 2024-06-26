select top 20 from (select id, count(distinct pubid) as pub_count from authored group by id) order by pub_count;