package io.github.github;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;

import java.sql.Connection;
import java.sql.DriverManager;

public class CmdConnect implements Cmd {

    // private OtpErlangTuple cmd;

    private Connection conn;

    private String driver; // "com.mysql.jdbc.Driver"
    private String url;
    private String username;
    private String password;

    public int getCmdId() {
        return 1;
    }
/*
    public OtpErlangTuple getCmdTerm() {
        return cmd;
    }
*/
    public CmdConnect decode(int cmdId, OtpErlangObject term) {
        CmdConnect c = new CmdConnect();
        OtpErlangTuple cmd = (OtpErlangTuple) term;
        c.driver = cmd.elementAt(0).toString();
        c.url = cmd.elementAt(1).toString();
        c.username = cmd.elementAt(2).toString();
        c.password = cmd.elementAt(3).toString();
        return c;
    }

    public void doCmd(ConnectionContext context) throws Exception {
        Class.forName(driver);
        conn = DriverManager.getConnection(url, username, password);
        context.setConnection(conn);
    }
}
