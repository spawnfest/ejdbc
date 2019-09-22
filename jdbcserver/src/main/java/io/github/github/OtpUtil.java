package io.github.github;

import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangObject;

import java.io.UnsupportedEncodingException;

public class OtpUtil {
    public static String getUtf8String(OtpErlangObject o) throws UnsupportedEncodingException {
        OtpErlangBinary b = (OtpErlangBinary)o;
        return new String(b.binaryValue(), "UTF-8" );
    }
}
