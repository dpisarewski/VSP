package mware_lib;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Created by pisare_d on 05.12.13.
 */
public class Connection {

    private static final Logger logger = Logger.getLogger( Connection.class.getName() );

    String hostname;
    Socket socket;
    int port;

    public Socket getSocket() {
        return socket;
    }

    public void setSocket(Socket socket) {
        this.socket = socket;
    }

    public String getHostname() {
        return hostname;
    }

    public void setHostname(String hostname) {
        this.hostname = hostname;
    }

    public int getPort() {
        return port;
    }

    public void setPort(int port) {
        this.port = port;
    }

    public Connection(String hostname, int port){
        this.hostname   = hostname;
        this.port       = port;
        try {
            socket = new Socket(hostname, port);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public Connection(Socket socket){
        this.socket     = socket;
        this.hostname   = socket.getInetAddress().getHostAddress();
        this.port       = socket.getPort();
    }

    public void send(String message) throws IOException {
        logger.info("Sending message to " + hostname + ":" + port + ": " + message);
        socket.getOutputStream().write((message + "\n").getBytes());
    }

    public void sendAndClose(String message){
        try {
            send(message);
            close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String sendAndRead(String message){
        try {
            send(message);
            String result = readAll();
            close();
            return result;
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public BufferedReader getReader() throws IOException {
        return new BufferedReader(new InputStreamReader(socket.getInputStream()));
    }

    public void close() throws IOException {
        socket.close();
    }

    public String readAll() throws IOException {
        return getReader().readLine();
    }




}
