package mware_lib;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Created by pisare_d on 12.12.13.
 */
public class Proxy {

    private static final Logger logger = Logger.getLogger( Proxy.class.getName() );

    public static List<Object> invoke(String hostname, int port, String name, String methodName, Object object){
        logger.info("Invoking method " + methodName + " on object " + name + " on " + hostname + ":" + port);
        Connection connection = new Connection(hostname, port);
        try {
            String result = connection.sendAndRead(Marshalling.encodeInvoke(name, methodName, object));
            logger.info("Received response: " + result);
            return Marshalling.decodeResult(result);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
            try {
                connection.sendAndClose(Marshalling.encodeResult(e));
            } catch (IOException e1) {
                e1.printStackTrace();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return null;
    }
}
