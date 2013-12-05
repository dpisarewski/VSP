package name_service;

import mware_lib.Connection;

import java.io.BufferedReader;
import java.io.IOException;
import java.net.ServerSocket;
import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 16:11
 */
public class NameServiceServer extends Thread{

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
        String line;
        String command;
        String name;
        String object;
        Connection connection;
        BufferedReader reader;

        while(true){
            try {
                connection  = new Connection(socket.accept());
                reader      = connection.getReader();
                while((line  = reader.readLine()) != null){
                    command = line.split("#")[0];
                    name    = line.split("#")[1];
                    object  = line.split("#")[2];

                    switch(command){
                        case "REBIND":
                            registerObject(name, object);
                            connection.send("OK");
                            break;
                        case "RESOLVE":
                            connection.send("OK#" + getObject(name));
                            break;
                    }
                }
                connection.close();
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
