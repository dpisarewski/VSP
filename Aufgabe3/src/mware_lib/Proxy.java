package mware_lib;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Created by pisare_d on 12.12.13.
 */
public class Proxy {

    private static final Logger logger = Logger.getLogger( Proxy.class.getName() );

    public static Map<String, Object> invoke(String hostname, int port, String name, String methodName, Object object) throws Exception {
        logger.info("Invoking method " + methodName + " on object " + name + " from " + hostname + ":" + port);
        Connection connection = new Connection(hostname, port);

        String result = connection.sendAndRead(Marshalling.encodeInvoke(name, methodName, object));
        logger.info("Received response: " + result);
        Map<String, Object> resultObjects = Marshalling.unmarshall(result);
        raiseException(resultObjects);
        return resultObjects;
    }

    private static void raiseException(Map<String, Object> result) throws Exception {
        for(Object object : (ArrayList<Object>) result.get("params")){
            if(object instanceof Exception){
                Exception exception = (Exception) object;
                String exceptionClassName = exception.getMessage().split("#")[0].split("\\.")[1];
                logger.info("Converting " + exception.getMessage() + " " + exceptionClassName);
                Constructor<?> constructor = Class.forName(getExceptionClassName(exceptionClassName)).getConstructor(new Class[]{String.class});
                throw (Exception) constructor.newInstance(exception.getMessage());
            }
        }
    }

    private static boolean checkClass(String className){
        try {
            Class.forName(className);
        } catch( ClassNotFoundException e ) {
            return false;
        }
        return true;
    }

    private static String getExceptionClassName(String name){
        String[] packages = new String[]{"bank_access", "cash_access"};
        for(String pack: packages){
            if(checkClass(pack + "." + name)) return pack + "." + name;
        }
        return "java.lang.RuntimeException";
    }
}
