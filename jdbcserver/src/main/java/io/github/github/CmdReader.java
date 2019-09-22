package io.github.github;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpInputStream;

import java.io.IOException;
import java.io.InputStream;
import java.util.HashMap;
import java.util.Map;

public class CmdReader {
    private final InputStream is;
    private final Map<Integer, CmdDecoder> cmdsMap = new HashMap<Integer, CmdDecoder>();
    public CmdReader(InputStream is) {
        this.is = is;
        putCmd(new CmdConnect());
    }

    private void putCmd(Cmd cmd) {
        cmdsMap.put(cmd.getCmdId(), cmd);
    }

    public Cmd getReadCmd() throws Exception {
        int cmdId = readByte();
        int lengthByte1 = readByte();
        int lengthByte2 = readByte();
        int length = ((lengthByte1<<8 & 0xff00) | (lengthByte2 & 0x00ff));
        CmdDecoder cmdDecoder = cmdsMap.get(cmdId);
        if (cmdDecoder != null ) {
            byte[] data = new byte[length];
            for (int i = 0; i < length; i++) {
                data[i] = (byte) readByte();
            }
            OtpErlangObject term = OtpErlangObject.decode(new OtpInputStream(data));
            return cmdDecoder.decode(cmdId, term);
        } else {
            is.skip(length);
            throw new Exception("Invalid ejdbc commandId=" + cmdId);
        }
    }

    private int readByte() throws IOException {
        int b = -1;
        while(b == -1) {
            b = is.read();
        }
        return b;
    }
}
