-- Author
create table if not exists tempAuthor (k TEXT, name TEXT);
insert into tempAuthor (select f.k, f.v from Field f where f.p = 'author');
-- add a primary key that's an int:
create sequence seq;
insert into Author (select nextval('seq') as id, k as tempkey, name from tempAuthor);
drop sequence seq;

-- Publication
create table if not exists tempPublication (k TEXT, title TEXT, year TEXT);
insert into tempPublication (select f.k, f.v, g.v from Field f, Field g where f.k = g.k and f.p = 'title' and g.p = 'year');
-- add a primary key that's an int:
create sequence seq;
insert into Publication (select nextval('seq') as pubid, k as pubkey, title, year from tempPublication);
drop sequence seq;

-- Authored
insert into Authored (select a.id, p.pubid from Author a join Publication p on a.tempkey = p.pubkey);
alter table Author drop column tempkey;

-- Article
create table if not exists tempArticle (k TEXT, journal TEXT, month TEXT, volume TEXT, number TEXT);
insert into tempArticle (select f.k, f.v, g.v, h.v, i.v from Field f, Field g, Field h, Field i, Pub p where p.p = 'article' and f.k = g.k and f.k = h.k and f.k = i.k and f.p = 'journal' and g.p = 'month' and h.p = 'volume' and i.p = 'number');
insert into Article (select p.pubid, a.journal, a.month, a.volume, a.number from Publication p join tempArticle a on p.pubkey = a.k);

-- Book
create table if not exists tempBook (k TEXT, publisher TEXT, isbn TEXT);
insert into tempBook (select f.k, f.v, g.v from Field f, Field g, Pub p where p.p = 'book' and f.k = g.k and f.p = 'publisher' and g.p = 'isbn');
insert into Book (select p.pubid, b.publisher, b.isbn from Publication p join tempBook b on p.pubkey = b.key);

-- Incollection
create table if not exists tempIncollection (k TEXT, booktitle TEXT, publisher TEXT, isbn TEXT);
insert into tempIncollection (select f.k, f.v, g.v, h.v from Field f, Field g, Field h, Pub p where p.p = 'incollection' and f.k = g.k and f.k = h.k and f.p = 'booktitle' and g.p = 'publisher' and h.p = 'isbn');
insert into Incollection (select p.pubid, i.booktitle, i.publisher, i.isbn from Publication p join tempIncollection i on p.pubkey = i.k);

-- Inproceedings
create table if not exists tempInproceedings (k TEXT, booktitle TEXT, editor TEXT);
insert into tempInproceedings (select f.k, f.v, g.v from Field f, Field g, Pub p where p.p = 'inproceedings' and f.k = g.k and f.p = 'booktitle' and g.p = 'editor');
insert into Inproceedings (select p.pubid, i.booktitle, i.editor from Publication p join tempInproceedings i on p.pubkey = i.k);


-- Create key and unique constraints
alter table Author add primary key (id);
alter table Publication add primary key (pubid);
alter table Publication add constraint pubkey_unique unique (pubkey);
alter table Authored add foreign key (id) references Author);
alter table Authored add foreign key (pubid) references Publication;
alter table Authored add primary key (id, pubid);
alter table Article add foreign key (pubid) references Publication;
alter table Article add primary key (pubid);
alter table Book add foreign key (pubid) references Publication;
alter table Book add primary key (pubid);
alter table Incollection add foreign key (pubid) references Publication;
alter table Incollection add primary key (pubid);
alter table Inproceedings add foreign key (pubid) references Publication;
alter table Inproceedings add primary key (pubid);


-- 'DROP TABLE' commands
drop table if exists tempAuthor;
drop table if exists tempPublication;
drop table if exists tempArticle;
drop table if exists tempBook;
drop table if exists tempIncollection;
drop table if exists tempInproceedings;
