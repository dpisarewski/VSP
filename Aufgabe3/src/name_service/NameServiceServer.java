package name_service;

import mware_lib.Connection;
import mware_lib.Marshalling;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 16:11
 */
public class NameServiceServer extends Thread{

    private static final Logger logger = Logger.getLogger( NameServiceServer.class.getName() );

    ServerSocket socket;
    Map<String, Map> registry;
    int port;

    public NameServiceServer(int port) {
        registry  = new HashMap<>();
        this.port = port;
    }

    public static void main(String[] args){
        int port = Integer.valueOf(args[0]);
        (new NameServiceServer(port)).start();
    }

    public void run(){

        try {
            socket      = new ServerSocket(port);
            logger.info("Started NameService on port " + port);

            while(true){
                Connection connection = new Connection(socket.accept());
                (new NameServiceThread(this, connection)).start();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    synchronized void registerObject(String name, Map object){
        registry.put(name, object);
    }

    synchronized Map getObject(String name){
        return registry.get(name);
    }


}
