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

    final public static String RESOLVE  = "RESOLVE";
    final public static String REBIND   = "REBIND";

    public NameServiceServer(int port) throws IOException {
        registry    = new HashMap<>();
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
        String name;
        logger.info("Started NameService on port " + socket.getLocalPort());
        Connection connection;

        while(true){
            try {
                connection  = new Connection(socket.accept());
                String request     = connection.readAll();
                Map<String, Object> objectValues = Marshalling.unmarshall(request);
                String command = (String) objectValues.get("command");
                switch(command){
                    case "REBIND":
                        name = (String) objectValues.get("name");

                        Map<String, Object> objectData = new HashMap<>();
                        objectData.put("name", name);
                        objectData.put("hostname", objectValues.get("hostname"));
                        objectData.put("port", objectValues.get("port"));
                        objectData.put("classname", objectValues.get("classname"));

                        logger.info("Received rebind for object " + name + ": " + objectData);
                        registerObject(name, objectData);
                        connection.sendAndClose("OK");
                        break;
                    case "RESOLVE":
                        name = (String) objectValues.get("name");

                        logger.info("Received resolve for object " + name);
                        connection.sendAndClose(Marshalling.encodeResolveResponse(getObject(name)));
                        break;
                    default:
                        connection.close();
                }
            } catch (IOException e) {
                e.printStackTrace();
            } catch (ClassNotFoundException e) {
                e.printStackTrace();
            }
        }
    }

    private void registerObject(String name, Map object){
        registry.put(name, object);
    }

    private Map getObject(String name){
        return registry.get(name);
    }


}
