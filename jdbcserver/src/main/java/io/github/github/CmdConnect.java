package io.github.github;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;

import java.io.UnsupportedEncodingException;
import java.sql.Connection;
import java.sql.DriverManager;

public class CmdConnect implements Cmd {

    private String driver; // "com.mysql.jdbc.Driver"
    private String url;
    private String username;
    private String password;

    @Override
    public int getCmdId() {
        return 1;
    }

    @Override
    public CmdConnect decode(int cmdId, OtpErlangObject term) throws Exception {
        CmdConnect c = new CmdConnect();
        OtpErlangTuple cmd = (OtpErlangTuple) term;
        c.driver = getUtf8String(cmd.elementAt(0));
        c.url = getUtf8String(cmd.elementAt(1));
        c.username = getUtf8String(cmd.elementAt(2));
        c.password = getUtf8String(cmd.elementAt(3));
        return c;
    }

    private String getUtf8String(OtpErlangObject o) throws UnsupportedEncodingException {
        OtpErlangBinary b = (OtpErlangBinary)o;
        return new String(b.binaryValue(), "UTF-8" );
    }

    @Override
    public Response doCmd(ConnectionContext context) throws Exception {
        Class.forName(driver);
        Connection conn = DriverManager.getConnection(url, username, password);
        context.setConnection(conn);
        return Response.OK;
    }
}
