package name_service;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.HashMap;
import java.util.Map;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 16:11
 */
public class NameService extends Thread{

    ServerSocket socket;
    Map<String, Object> registry;

    public NameService(int port) throws IOException {
        registry    = new HashMap<String, Object>();
        socket      = new ServerSocket(port);
    }

    public static void main(String[] args){
        int port            = Integer.valueOf(args[0]);

        try {
            (new NameService(port)).start();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    public void run(){
        String line;
        String command;
        Socket s;
        BufferedReader reader;

        while(true){
            try {
                s        = socket.accept();
                reader   = new BufferedReader(new InputStreamReader(s.getInputStream()));
                while((line  = reader.readLine()) != null){
                    command = line.split(":")[0];
                    switch(command){
                        case "REBIND":
                            String name = line.split(":")[1];
                            break;
                        case "RESOLVE":
                            break;
                    }
                }
            } catch (IOException e) {
                e.printStackTrace();
            }

        }
    }

    private void registerObject(String name, Object object){
        registry.put(name, object);
    }

    private Object getObject(String name){
        return registry.get(name);
    }


}
