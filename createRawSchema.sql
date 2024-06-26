drop table if exists Pub;
drop table if exists Field;

create table Pub (k text, p text);
create table Field (k text, i text, p text, v text);
\copy Pub from '/home/kvo241/cse544-kvo24/hw/hw1/submission/pubFile.txt';
\copy Field from '/home/kvo241/cse544-kvo24/hw/hw1/submission/fieldFile.txt'; 
