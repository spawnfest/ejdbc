package io.github.github;

import java.sql.Connection;
import java.sql.SQLException;

public class ConnectionContext {
    private Connection conn;

    private boolean end = false;
    public Connection getConnection() throws SQLException {
        if (conn == null) {
            throw new RuntimeException("Connection is not ready!");
        }
        if (conn.isClosed()) {
            throw new RuntimeException("Connection is closed!");
        }
        return conn;
    }

    public void setConnection(Connection conn) {
        if (this.conn != null) {
            throw new RuntimeException("Connection is already set!");
        }
        this.conn = conn;
    }

    public boolean isEnd() {
        return end;
    }

    public void setEnd(boolean end) {
        this.end = end;
    }
}
