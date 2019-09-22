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
        System.out.println("cmdId=" + cmdId);
        int lengthByte1 = readByte();
        System.out.println("byte1=" + lengthByte1);
        int lengthByte2 = readByte();
        System.out.println("byte2=" + lengthByte2);
        int length = ((lengthByte1<<8 & 0xff00) | (lengthByte2 & 0x00ff));
        System.out.println("length=" + length);
        CmdDecoder cmdDecoder = cmdsMap.get(cmdId);
        if (cmdDecoder != null ) {
            System.out.println("cmdDecoder=" + cmdDecoder.getClass().toString());
            byte[] data = new byte[length];
            for (int i = 0; i < length; i++) {
                data[i] = (byte) readByte();
                // System.out.println("byte["+i+"]=" + data[i]);
            }
            OtpErlangObject term = OtpErlangObject.decode(new OtpInputStream(data));
            System.out.println("erlang term=" + term.toString());
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
