package io.github.github;

import com.ericsson.otp.erlang.OtpErlangObject;

public class CmdExecuteQuery implements Cmd {
    @Override
    public int getCmdId() {
        return 0;
    }

    @Override
    public Response doCmd(ConnectionContext context) throws Exception {
        return null;
    }

    @Override
    public Cmd decode(int cmdId, OtpErlangObject term) throws Exception {
        return null;
    }
}
