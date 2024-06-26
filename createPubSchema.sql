-- 'DROP TABLE' commands
DROP TABLE IF EXISTS Author;
DROP TABLE IF EXISTS Publication;
DROP TABLE IF EXISTS Authored;
DROP TABLE IF EXISTS Article;
DROP TABLE IF EXISTS Book;
DROP TABLE IF EXISTS Incollection;
DROP TABLE IF EXISTS Inproceedings;

-- Create tables to implement the conceptual schema from the E/R diagram.
-- We will call this database schema the 'PubSchema'.

-- parent tables:
CREATE TABLE Author (id INT, tempkey TEXT, name TEXT); -- giving up on the homepage bit
CREATE TABLE Publication (pubid INT, pubkey TEXT, title TEXT, year TEXT);

-- relationship:
CREATE TABLE Authored (id INT, pubid INT);

-- subclasses:
CREATE TABLE Article (pubid INT, journal TEXT, month TEXT, volume TEXT, number TEXT);
CREATE TABLE Book (pubid INT, publisher TEXT, isbn TEXT);
CREATE TABLE Incollection (pubid INT, booktitle TEXT, publisher TEXT, isbn TEXT);
CREATE TABLE Inproceedings (pubid INT, booktitle TEXT, editor TEXT);



-- Just making note of this bizarre result:
-- dblp=# select count(*) from Field where k='journals/jgt/GouldLP04';
--  count 
-- -------
--  36047
-- (1 row)

-- dblp=# select count(*) from Pub where k='journals/jgt/GouldLP04';
--  count 
-- -------
--      1
-- (1 row)
