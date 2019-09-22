# ejdbc

ejdbc (Erlang JDBC) is Erlang bridge over JDBC API.

## Goal and purpose

 * Check to see if is viable solution to connect to legacy or old databases where we don't have viable connecting solution from erlang
 * Use JDBC conventions for database URL, sql query parameters
 * Code and design is inspired by erlang odbc aplication that use c-port driver and ODBC API

## Design


## How to compile

### Optional build jdbcserver

```
cd jdbcserver
mvn package
cp target/jdbcserver-1.0-SNAPSHOT.jar ../ejdbc/priv/jdbcserver.jar
cd ..
```

### Compile ejdbc

```
cd ejdbc
rebar3 compile
```

## How to use (ignore log messages)

```
cd ejdbc
export CLASSPATH=./priv/h2-1.4.199.jar
rebar3 shell

>  application:start(ejdbc).
>  {ok, Pid} = ejdbc:connect(<<"org.h2.Driver">>,<<"jdbc:h2:mem:">>,<<"sa">>,<<"">>, []).
{ok,<0.131.0>}
>
>  ejdbc:execute_query(Pid, <<"select * from test">>).
{error,<<"Table \"TEST\" not found; SQL statement:\nselect * from test [42102-199]">>}
>  ejdbc:execute_update(Pid, <<"create table test(id integer, name varchar(100))">>).
{ok,0}
>  ejdbc:execute_update(Pid, <<"insert into test values(1, 'a')">>).
{ok,1}
>  ejdbc:execute_update(Pid, <<"insert into test values(2, 'b')">>).
{ok,1}
>  ejdbc:execute_update(Pid, <<"insert into test values(3, null)">>).
{ok,1}
>  ejdbc:execute_query(Pid, <<"select * from test">>).
{ok,[[<<"1">>,<<"a">>],[<<"2">>,<<"b">>],[<<"3">>,nil]]}
>  ejdbc:execute_update(Pid, <<"update test set id=id+10">>).
{ok,3}
>  ejdbc:execute_query(Pid, <<"select count(*) from test where name is not null">>).
{ok,[[<<"2">>]]}

> ejdbc:disconnect(Pid).
ok
```

