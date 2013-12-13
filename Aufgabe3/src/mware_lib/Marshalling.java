package mware_lib;

import bank_access.OverdraftException;
import cash_access.InvalidParamException;
import org.apache.commons.codec.binary.Base64;

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
        List<Object> objects = convertObject(object);
        logger.info("Marshalling objects: " + objects.toString());
        ByteArrayOutputStream outputStream      = new ByteArrayOutputStream();
        ObjectOutputStream objectOutputStream   = new ObjectOutputStream(outputStream);
        objectOutputStream.writeObject(objects);
        return new String(Base64.encodeBase64(outputStream.toByteArray()));
    }

    public static List<Object> unmarshall(String params) throws IOException, ClassNotFoundException {
        ArrayList<Object> result = new ArrayList<Object>();
        ByteArrayInputStream inputStream    = new ByteArrayInputStream(Base64.decodeBase64(params));
        ObjectInputStream objectInputStream = new ObjectInputStream(inputStream);
        while(true){
            try{
                result = (ArrayList<Object>) objectInputStream.readObject();
            } catch (EOFException e){
                break;
            }
        }
        logger.info("Unmarshalled objects: " + result);
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

    private static List<Object> convertObject(Object object){
        convertException(object);
        List<Object> objects = new ArrayList<Object>();
        if (!(object instanceof List)){
            objects.add(object);
        } else{
            objects = (ArrayList<Object>) object;
        }
        return objects;
    }

    private static void convertException(Object object){
        if (object instanceof Exception && !(object instanceof InvalidParamException) && !(object instanceof OverdraftException)){
            logger.info("Converting exception " + object.toString() + " into RuntimeException");
            object = new RuntimeException((Exception)object);
        }
    }
}
