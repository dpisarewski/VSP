package name_service;

import mware_lib.Connection;
import mware_lib.Marshalling;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Created by pisare_d on 19.12.13.
 */
public class NameServiceThread extends Thread {

    private static final Logger logger = Logger.getLogger( NameServiceServer.class.getName() );

    Connection connection;
    NameServiceServer server;

    public NameServiceThread(NameServiceServer server, Connection connection){
        this.server     = server;
        this.connection = connection;
    }

    public void run(){
        String request;
        String name;
        try {
            request = connection.readAll();

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
                    server.registerObject(name, objectData);
                    connection.sendAndClose("OK");
                    break;
                case "RESOLVE":
                    name = (String) objectValues.get("name");

                    logger.info("Received resolve for object " + name);
                    connection.sendAndClose(Marshalling.encodeResolveResponse(server.getObject(name)));
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
