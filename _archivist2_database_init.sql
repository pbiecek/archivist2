#
# init mysql database on server side [remote repo]

create database archivist2;
use archivist2;

create table IF NOT EXISTS artifact (
md5hash VARCHAR(35),
name VARCHAR(512),
createdDate VARCHAR(25),
donorIP VARCHAR(20)
);

create table IF NOT EXISTS tag (
md5hash VARCHAR(35),
tag TEXT,
createdDate VARCHAR(25)
);

