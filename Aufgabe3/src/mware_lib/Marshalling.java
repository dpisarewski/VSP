package mware_lib;

import bank_access.OverdraftException;
import cash_access.InvalidParamException;

import java.io.*;
import java.util.*;
import java.util.logging.Logger;

/**
 * Created by pisare_d on 12.12.13.
 */
public class Marshalling {

    private static final Logger logger = Logger.getLogger( Marshalling.class.getName() );

    private static final String INVOKE = "INVOKE";
    private static final String RESULT = "RESULT";

    public static String marshall(Object object) throws IOException {
        convertException(object);
        logger.info("Marshalling objects: " + object.toString());
        ByteArrayOutputStream outputStream      = new ByteArrayOutputStream();
        ObjectOutputStream objectOutputStream   = new ObjectOutputStream(outputStream);
        objectOutputStream.writeObject(object);
        return new String(new Base64().encode(outputStream.toByteArray()));
    }

    public static Map<String, Object> unmarshall(String params) throws IOException, ClassNotFoundException {
        Map<String, Object> result = new HashMap<>();
        ByteArrayInputStream inputStream    = new ByteArrayInputStream(new Base64().decode(params));
        ObjectInputStream objectInputStream = new ObjectInputStream(inputStream);
        while(true){
            try{
                result = (Map<String, Object>) objectInputStream.readObject();
            } catch (EOFException e){
                break;
            }
        }
        logger.info("Unmarshalled objects: " + result);
        return result;
    }

    public static String encodeInvoke(String name, String methodName, Object object) throws IOException {
        Map<String, Object> request = new HashMap<>();
        request.put("command", INVOKE);
        request.put("name", name);
        request.put("method", methodName);
        request.put("params", convertObject(object));
        return Marshalling.marshall(request);
    }

    public static String encodeResult(Object object) throws IOException {
        Map<String, Object> request = new HashMap<>();
        List<Object> params = new ArrayList<Object>();
        params.add(object);
        request.put("command", RESULT);
        request.put("params", params);
        return Marshalling.marshall(request);
    }

    private static void convertException(Object object){
        if (object instanceof Exception && !(object instanceof InvalidParamException) && !(object instanceof OverdraftException)){
            logger.info("Converting exception " + object.toString() + " into RuntimeException");
            object = new RuntimeException((Exception)object);
        }
    }

    private static List<Object> convertObject(Object object){
        convertException(object);
        List<Object> objects = new ArrayList<Object>();
        if(object == null) return objects;

        if (!(object instanceof List)){
            objects.add(object);
        } else{
            objects = (ArrayList<Object>) object;
        }
        return objects;
    }
}
