package mware_lib;

import bank_access.OverdraftException;
import cash_access.InvalidParamException;

import java.io.*;
import java.util.*;
import java.util.logging.Level;
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
        List<Object> objects = new ArrayList<Object>();
        objects.add(object);
        return marshall(objects);
    }

    private static String marshall(List<Object> objects) throws IOException {
        logger.log(Level.INFO, "Marshalling objects: " + objects.toString());
        ByteArrayOutputStream outputStream      = new ByteArrayOutputStream();
        ObjectOutputStream objectOutputStream   = new ObjectOutputStream(outputStream);
        objectOutputStream.writeObject(objects);
        return Base64.encode(outputStream.toByteArray());
    }

    public static List<Object> unmarshall(String params) throws IOException, ClassNotFoundException {
        logger.log(Level.INFO, "Unmarshalling objects: " + params);
        ArrayList<Object> result            = new ArrayList<Object>();
        ByteArrayInputStream inputStream    = new ByteArrayInputStream(Base64.decode(params));
        ObjectInputStream objectInputStream = new ObjectInputStream(inputStream);
        while(true){
            try{
                result.add(objectInputStream.readObject());
            } catch (EOFException e){
                break;
            }
        }
        return result;
    }

    public static String encodeInvoke(String name, String methodName, Object object) throws IOException {
        return INVOKE + "#" + name + "#" + methodName + "#" + Marshalling.marshall(object);
    }

    public static Map decodeInvoke(String request) throws IOException, ClassNotFoundException {
        String name     = request.split("#")[1];
        String method   = request.split("#")[2];
        String params   = request.split("#")[3];
        List<Object> parameters = Marshalling.unmarshall(params);
        Map<String, Object> result = new HashMap<String, Object>();
        result.put("name", name);
        result.put("method", method);
        result.put("params", parameters);
        return result;
    }

    public static String encodeResult(Object object) throws IOException {
        return RESULT + "#" + Marshalling.marshall(object);
    }

    public static List<Object> decodeResult(String result) throws IOException, ClassNotFoundException {
        String params   = result.split("#")[1];
        return Marshalling.unmarshall(params);
    }

    private static void convertException(Object object){
        if (object instanceof Exception && !(object instanceof InvalidParamException) && !(object instanceof OverdraftException)){
            logger.log(Level.INFO, "Converting exception " + object.toString() + " into RuntimeException");
            object = new RuntimeException((Exception)object);
        }
    }
}
