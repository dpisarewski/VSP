package mware_lib;

import java.io.IOException;
import java.util.ArrayList;

/**
 * Created by pisare_d on 12.12.13.
 */
public class Proxy {

    public static ArrayList<Object> invoke(String hostname, int port, String name, String methodName, Object object){
        Connection connection = new Connection(hostname, port);
        try {
            connection.send(Marshalling.encodeInvoke(name, methodName, object));
            String result = connection.readAll();
            return Marshalling.decodeResult(result);
        } catch (IOException | ClassNotFoundException e) {
            e.printStackTrace();
            try {
                connection.send(Marshalling.encodeResult(e));
            } catch (IOException e1) {
                e1.printStackTrace();
            }
        }
        return null;
    }
}
