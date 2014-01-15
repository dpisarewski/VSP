package mware_lib;

import bank_access.OverdraftException;
import cash_access.InvalidParamException;
import name_service.NameServiceServer;

import java.io.*;
import java.util.*;
import java.util.logging.Logger;

/**
 * Created by pisare_d on 12.12.13.
 */
public class Marshalling {

    private static final Logger logger = Logger.getLogger( Marshalling.class.getName() );

    private static final String INVOKE  = "INVOKE";
    private static final String RESULT  = "RESULT";
    private static final String REBIND  = "REBIND";
    private static final String RESOLVE = "RESOLVE";

    public static String marshall(Object object) throws IOException {
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
        request.put("command", RESULT);
        request.put("params", convertObject(object));
        return Marshalling.marshall(request);
    }

    public static String encodeRebind(String name, String classname, String hostname, int port) throws IOException {
        Map<String, Object> request = new HashMap<>();
        request.put("command", REBIND);
        request.put("name", name);
        request.put("classname", classname);
        request.put("hostname", hostname);
        request.put("port", port);
        return Marshalling.marshall(request);
    }

    public static String encodeResolve(String name) throws IOException {
        Map<String, Object> request = new HashMap<>();
        request.put("command", RESOLVE);
        request.put("name", name);
        return Marshalling.marshall(request);
    }

    public static String encodeResolveResponse(Map<String, Object> objectData) throws IOException {
        Map<String, Object> request = new HashMap<>();
        request.put("command", RESULT);
        if(objectData != null) request.putAll(objectData);
        return Marshalling.marshall(request);
    }

    private static Object convertException(Object object){
        if (object instanceof Exception){
            logger.info("Converting exception " + object.toString() + " into RuntimeException");
            Exception exception = (Exception)object;
            return new RuntimeException(exception.getClass().getName() + "#" +  exception.getMessage());
        }
        return object;
    }

    private static List<Object> convertObject(Object object){
        object = convertException(object);
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
