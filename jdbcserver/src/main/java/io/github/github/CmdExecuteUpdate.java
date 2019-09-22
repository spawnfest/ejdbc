package io.github.github;

import com.ericsson.otp.erlang.*;

import java.nio.charset.Charset;
import java.sql.*;
import java.util.ArrayList;
import java.util.List;

public class CmdExecuteUpdate implements Cmd {
    
    private String sql;
    
    @Override
    public int getCmdId() {
        return 4;
    }

    @Override
    public Cmd decode(int cmdId, OtpErlangObject term) throws Exception {
        CmdExecuteUpdate q = new CmdExecuteUpdate();
        q.sql = OtpUtil.getUtf8String(term);
        return q;
    }

    @Override
    public Response doCmd(ConnectionContext context) throws Exception {
        Connection conn = context.getConnection();
        PreparedStatement ps = null;
        try {
            ps = conn.prepareStatement(sql);
            // @return either (1) the row count for SQL Data Manipulation Language (DML) statements
            //             or (2) 0 for SQL statements that return nothing
            int updated = ps.executeUpdate();
            OtpErlangInt data = new OtpErlangInt(updated);
            ps.close();
            ps = null;
            return Response.okData(data);
        } finally {
            if (ps != null) {
                try {
                    ps.close();
                } catch (SQLException e) {
                }
            }
        }
    }
}
