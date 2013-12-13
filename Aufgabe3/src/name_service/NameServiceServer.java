package name_service;

import mware_lib.Connection;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
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
    Map<String, String> registry;

    final public static String RESOLVE  = "RESOLVE";
    final public static String REBIND   = "REBIND";

    public NameServiceServer(int port) throws IOException {
        registry    = new HashMap<String, String>();
        socket      = new ServerSocket(port);
    }

    public static void main(String[] args){
        int port            = Integer.valueOf(args[0]);

        try {
            (new NameServiceServer(port)).start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void run(){
        logger.log(Level.INFO, "Started NameService on port " + socket.getLocalPort());
        String line;
        String command;
        String name;
        String object;
        Connection connection;
        BufferedReader reader;

        while(true){
            try {
                connection  = new Connection(socket.accept());
                line        = connection.readAll();

                command = line.split("#")[0];
                name    = line.split("#")[1];

                switch(command){
                    case "REBIND":
                        object  = line.split("#")[2];
                        logger.log(Level.INFO, "Received rebind for object " + name + ": " + object);
                        registerObject(name, object);
                        connection.sendAndClose("OK");
                        break;
                    case "RESOLVE":
                        logger.log(Level.INFO, "Received resolve for object " + name);
                        connection.sendAndClose("OK#" + name + "#" + getObject(name));
                        break;
                }

            } catch (IOException e) {
                e.printStackTrace();
            }
        }
    }

    private void registerObject(String name, String object){
        registry.put(name, object);
    }

    private String getObject(String name){
        return registry.get(name);
    }


}
