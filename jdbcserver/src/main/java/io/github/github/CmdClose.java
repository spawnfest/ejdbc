package io.github.github;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import java.io.UnsupportedEncodingException;
import java.sql.Connection;
import java.sql.DriverManager;

public class CmdClose implements Cmd {

    @Override
    public int getCmdId() {
        return 2;
    }

    @Override
    public CmdClose decode(int cmdId, OtpErlangObject term) throws Exception {
        CmdClose c = new CmdClose();
        return c;
    }

    @Override
    public Response doCmd(ConnectionContext context) throws Exception {
        context.getConnection().close();
        context.setEnd(true);
        return Response.OK;
    }
}