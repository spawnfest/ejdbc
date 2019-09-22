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
>  TODO execute sql
>
>
> ejdbc:disconnect(Pid).
ok
```

