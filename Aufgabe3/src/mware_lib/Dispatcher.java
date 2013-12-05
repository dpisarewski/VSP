package mware_lib;

import java.io.IOException;
import java.net.Inet4Address;
import java.net.ServerSocket;
import java.net.UnknownHostException;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 15:36
 */
public class Dispatcher extends Thread{

    public final static int PORT = 20000;
    public static String hostname;

    private Dispatcher instance = new Dispatcher();

    private Dispatcher(){
        if(hostname == null){
            try {
                hostname = Inet4Address.getLocalHost().getHostAddress();
            } catch (UnknownHostException e) {
                e.printStackTrace();
            }
        }
        start();
    }

    public void run(){
        try {
            while(true){
                ServerSocket socket     = new ServerSocket(PORT);
                Connection connection   = new Connection(socket.accept());
                new Skeleton(connection).start();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public Dispatcher getInstance(){
        return instance;
    }

}
