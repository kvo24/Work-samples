create index if not exists index_on_Pub_p on Pub(p);
select p as publication_type, COUNT(*) as count from Pub group by p;

/* Answer to query 1:

publication_type  |  count  
---------------+---------
 article       | 3379968
 book          |   20333
 incollection  |   70301
 inproceedings | 3394712
 mastersthesis |      21
 phdthesis     |  125554
 proceedings   |   57298
 www           | 3424188

 */


create index if not exists index_on_Pub_k on Pub(k);
create index if not exists index_on_Field_k on Field(k);
cluster Pub using index_on_Pub_k;
cluster Field using index_on_Field_k;
select Field.p as field_name from Field join Pub on Pub.k = Field.k group by Field.p having count(distinct Pub.p) = (select count(distinct Pub.p) from Pub);

/* Answer to query 2:

field_name 
------------
 author
 ee
 note
 title
 year

*/



/* For reference, a list of all distinct field names in Field.p:

 year
 editor
 url
 title
 month
 isbn
 cite
 volume
 number
 address
 school
 booktitle
 publisher
 ee
 pages
 journal
 note
 author
 cdrom
 series
 chapter
 crossref

*/

create index if not exists index_on_Field_p on Field(p);
create index if not exists index_on_Field_v on Field(v);


-- Analytic query:
select top 20 from (select id, count(distinct pubid) as pub_count from authored group by id) order by pub_count;
