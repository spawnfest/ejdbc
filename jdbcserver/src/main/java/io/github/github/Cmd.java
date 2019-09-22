package io.github.github;

import com.ericsson.otp.erlang.OtpErlangObject;

import java.sql.Connection;

public interface Cmd extends CmdDecoder {
    public int getCmdId();
    // public T getCmdTerm();
    public void doCmd(ConnectionContext context) throws Exception;
}