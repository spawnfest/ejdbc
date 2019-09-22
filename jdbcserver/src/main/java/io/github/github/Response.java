package io.github.github;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.charset.Charset;


public class Response {
    public static Response OK = new Response((byte)1);
    //
    private final byte response;
    private final byte[] data;

    public Response(byte response) {
        this(response, null);
    }

    public Response(byte response, byte[] data) {
        this.response = response;
        this.data = data;
    }

    public void send(OutputStream out) throws IOException {
        out.write(response);
        if (data != null) {
            int length = data.length;
            byte[] lengthByte = ByteBuffer.allocate(4).order(ByteOrder.BIG_ENDIAN).putInt(length).array();
            out.write(lengthByte);
            out.write(data);
        }
        out.flush();
    }

    public static Response error(String errorMessage) {
        byte[] bytes = errorMessage.getBytes(Charset.forName("utf-8"));
        Response error = new Response((byte)2, bytes);
        return error;
    }
/*
    private static Response ok() {
        Response ok = new Response((byte)1);
        return ok;
    }

 */
}
