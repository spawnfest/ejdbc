#!/bin/sh

rm target/*.jar
mvn package
cp target/jdbcserver-1.0-SNAPSHOT.jar ../ejdbc/priv/jdbcserver.jar
java -cp target/jdbcserver-1.0-SNAPSHOT.jar:../ejdbc/priv/jinterface-1.6.1.jar io.github.github.JdbcServer

