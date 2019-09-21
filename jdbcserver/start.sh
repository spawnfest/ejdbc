#!/bin/sh

mvn package
java -cp target/jdbcserver-1.0-SNAPSHOT.jar io.github.github.JdbcServer

