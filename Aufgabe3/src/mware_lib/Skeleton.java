package mware_lib;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 15:36
 */
public class Skeleton extends Thread{

    private static final Logger logger = Logger.getLogger( Skeleton.class.getName() );

    private Connection connection;

    public Skeleton(Connection connection){
        this.connection = connection;
    }

    public void run(){
        logger.info("Started new Skeleton");
        try{
            String request = connection.readAll();
            logger.info("Received request " + request + " from " + connection.getHostname() + ":" + connection.getPort());
            if (request != null){
                String command  = (String) Marshalling.unmarshall(request).get("command");
                switch(command){
                    case "INVOKE":
                        connection.sendAndClose(invoke(request));
                        break;
                    default:
                        connection.close();
                }
            }
        } catch (Exception e){
            try {
                e.printStackTrace();
                connection.sendAndClose(Marshalling.encodeResult(e));
            } catch (IOException e1) {
                e1.printStackTrace();
                System.exit(1);
            }
        }
    }

    private synchronized String invoke(String request) throws IOException, ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ObjectBroker objectBroker = ObjectBroker.getInstance();
        Map methodCall  = Marshalling.unmarshall(request);
        String name     = (String) methodCall.get("name");
        String method   = (String) methodCall.get("method");
        ArrayList<Object> parameters        = (ArrayList<Object>) methodCall.get("params");
        ArrayList<Class> parameterClasses   = collectClasses(parameters);
        logger.info("Invoking locally method " + method + " on object " + name + " with parameters: " + parameters.toString());
        Object object   = objectBroker.getObject(name);
        Method m        = object.getClass().getDeclaredMethod(method, parameterClasses.toArray(new Class[parameterClasses.size()]));
        Object result   = m.invoke(object, parameters.toArray());
        return Marshalling.encodeResult(result);
    }

    private ArrayList<Class> collectClasses(List<Object> params){
        ArrayList<Class> classes = new ArrayList<>();
        for(Object param : params){
            classes.add(findClass(param));
        }
        return classes;
    }

    private Class findClass(Object param){
        if (param instanceof Double){
            return Double.TYPE;
        } else if(param instanceof Integer){
            return Integer.TYPE;
        } else if(param instanceof Boolean){
            return Boolean.TYPE;
        } else if(param instanceof Byte){
            return Byte.TYPE;
        } else if(param instanceof Character){
            return Character.TYPE;
        } else if(param instanceof Long){
            return Long.TYPE;
        } else if(param instanceof Short){
            return Short.TYPE;
        }else{
           return param.getClass();
        }
    }

}
