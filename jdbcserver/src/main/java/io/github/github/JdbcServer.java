package io.github.github;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Date;
import java.util.Scanner;

/**
 * Hello world!
 *
 */
public class JdbcServer
{
    public static void main( String[] args ) throws IOException {
        // System.out.println( "Hello World!" );
        ServerSocket serverSocket = new ServerSocket(0);
        System.out.println("port="+serverSocket.getLocalPort());
        Socket socket = serverSocket.accept();
        PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
        CmdReader cmdReader = new CmdReader(socket.getInputStream());
        //java.net.SocketInputStream in = socket.getInputStream();

        // System.out.println("socket.getInputStream()" + in.getClass().toString());

        // Scanner inScanner = new Scanner(socket.getInputStream());

        ConnectionContext context = new ConnectionContext();
        serverSocket.close();
        boolean end = false;
        while (!context.isEnd()) {
            /*
            int a = in.available();
            byte[] data = new byte[a];
            in.read(data);
            if (a > 0) {
                System.out.println("data size=" + a);
            }*/
            try {
                Cmd cmd = cmdReader.getReadCmd();
                cmd.doCmd(context);
            } catch (Exception e) {

            }
        }



        out.flush();
        out.close();
        socket.close();
    }


}
