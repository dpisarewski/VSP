package mware_lib;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

/**
 * Created by pisare_d on 05.12.13.
 */
public class Connection {

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
        this.socket = socket;
    }

    public String send(String message){
        try {
            socket.getOutputStream().write(message.getBytes());
            return readAll();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public BufferedReader getReader(){
        try {
            return new BufferedReader(new InputStreamReader(socket.getInputStream()));
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }

    public void close(){
        try {
            socket.close();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public String readAll(){
        String result   = null;
        String line     = null;
        try {
            while((line = getReader().readLine()) != null){
                result  = (result == null) ? "" : result + line;
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return result;
    }




}
