package mware_lib;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 15:36
 */
public class Skeleton extends Thread{

    private Connection connection;

    public Skeleton(Connection connection){
        this.connection = connection;
    }

    public void run(){
        try{
            String request = connection.readAll();
            if (request != null){
                String command  = request.split("#")[0];
                switch(command){
                    case "INVOKE": invoke(request); break;
                }
            }
        } catch (Exception e){
            try {
                connection.send(encodeResult(e));
            } catch (IOException e1) {
                e1.printStackTrace();
                System.exit(1);
            }
        }
    }

    private String invoke(String request) throws IOException, ClassNotFoundException, NoSuchMethodException, InvocationTargetException, IllegalAccessException {
        ObjectBroker objectBroker = ObjectBroker.getInstance();
        String name     = request.split("#")[1];
        String method   = request.split("#")[2];
        String params   = request.split("#")[3];
        ArrayList<Object> parameters = Marshalling.unmarshall(params);
        Object object   = objectBroker.getObject(name);
        Method m        = object.getClass().getMethod(method, (Class<?>[]) parameters.toArray());
        Object result   = m.invoke(object, parameters);
        return encodeResult(result);
    }

    private String encodeResult(Object object) throws IOException {
        return "RESULT#" + Marshalling.marshall(object);
    }


}
