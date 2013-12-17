package mware_lib;

import java.io.IOException;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Created by pisare_d on 12.12.13.
 */
public class Proxy {

    private static final Logger logger = Logger.getLogger( Proxy.class.getName() );

    public static Map<String, Object> invoke(String hostname, int port, String name, String methodName, Object object){
        logger.info("Invoking method " + methodName + " on object " + name + " from " + hostname + ":" + port);
        Connection connection = new Connection(hostname, port);
        try {
            String result = connection.sendAndRead(Marshalling.encodeInvoke(name, methodName, object));
            logger.info("Received response: " + result);
            Map<String, Object> resultObjects = Marshalling.unmarshall(result);
            //raiseException(resultObjects);
            return resultObjects;
        } catch (Exception e) {
            e.printStackTrace();
            try {
                connection.sendAndClose(Marshalling.encodeResult(e));
            } catch (IOException e1) {
                e1.printStackTrace();
            }
        }
        return null;
    }

    private static void raiseException(List<Object> objects) throws Exception {
        for(Object object : objects){
            if(object instanceof Exception){
                throw (Exception) object;
            }
        }
    }
}
