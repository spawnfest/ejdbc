package io.github.github;

import com.ericsson.otp.erlang.OtpErlangObject;

public interface CmdDecoder {
    public Cmd decode(int cmdId, OtpErlangObject term);
}
