package io.github.github;

import com.ericsson.otp.erlang.*;

import java.nio.charset.Charset;
import java.sql.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class CmdExecuteQuery implements Cmd {
    
    private String sql;
    
    @Override
    public int getCmdId() {
        return 3;
    }

    @Override
    public Cmd decode(int cmdId, OtpErlangObject term) throws Exception {
        CmdExecuteQuery q = new CmdExecuteQuery();
        q.sql = OtpUtil.getUtf8String(term);
        return q;
    }

    @Override
    public Response doCmd(ConnectionContext context) throws Exception {
        Connection conn = context.getConnection();
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            ps = conn.prepareStatement(sql);
            rs = ps.executeQuery();
            ResultSetMetaData md = rs.getMetaData();
            List<OtpErlangList> rows = new ArrayList<OtpErlangList>();
            OtpErlangObject[] row;
            while (rs.next()) {
                row = new OtpErlangObject[md.getColumnCount()];
                for (int c = 1; c<=md.getColumnCount(); c++) {
                    String val = rs.getString(c);
                    if (rs.wasNull()) {
                        row[c-1] = new OtpErlangAtom("nil");
                    } else {
                        byte[] bytesUtf8 = val.getBytes(Charset.forName("utf-8"));
                        row[c-1] = new OtpErlangBinary(bytesUtf8);
                    }
                }
                rows.add(new OtpErlangList(row));
            }
            OtpErlangList[] rowsArr = new OtpErlangList[rows.size()];
            for(int r = 0; r<rows.size(); r++) {
                rowsArr[r] = rows.get(r);
            }
            OtpErlangList data = new OtpErlangList(rowsArr);
            rs.close();
            rs = null;
            ps.close();
            ps = null;
            return Response.okData(data);
        } finally {
            if (rs != null) {
                try {
                    rs.close();
                } catch (SQLException e) {
                }
            }
            if (ps != null) {
                try {
                    ps.close();
                } catch (SQLException e) {
                }
            }
        }
    }
}
