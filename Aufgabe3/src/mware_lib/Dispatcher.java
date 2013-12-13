package mware_lib;

import java.io.IOException;
import java.net.*;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 15:36
 */
public class Dispatcher extends Thread{

    private static final Logger logger = Logger.getLogger( Dispatcher.class.getName() );

    public final static int PORT = 20000;
    public static String hostname;
    private int port;
    private ServerSocket socket;

    private static Dispatcher instance = new Dispatcher();

    private Dispatcher(){
        logger.log(Level.INFO, "Starting Dispatcher");
        if(hostname == null){
            try {
                hostname    = Inet4Address.getLocalHost().getHostAddress();
            } catch (UnknownHostException e) {
                e.printStackTrace();
            }
            port        = PORT;
        }
    }

    public void run(){
        while(true){
            bind();
            Connection connection   = null;
            try {
                connection = new Connection(socket.accept());
            } catch (IOException e) {
                e.printStackTrace();
            }
            new Skeleton(connection).start();
        }
    }

    public static Dispatcher getInstance(){
        return instance;
    }

    private void bind(){
        try {
            socket = new ServerSocket(port);
            logger.log(Level.INFO, "Bound Dispatcher on port " + port);
        } catch (IOException e) {
            port++;
            bind();
        }
    }

    public int getPort() { return port; }


}
