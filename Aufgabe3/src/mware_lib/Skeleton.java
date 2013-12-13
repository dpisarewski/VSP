package mware_lib;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
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
                String command  = request.split("#")[0];
                switch(command){
                    case "INVOKE":
                        connection.sendAndClose(invoke(request));
                        break;
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

    private String invoke(String request) throws IOException, ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ObjectBroker objectBroker = ObjectBroker.getInstance();
        Map methodCall  = Marshalling.decodeInvoke(request);
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
            classes.add(param.getClass());
        }
        return classes;
    }


}
