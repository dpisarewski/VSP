package mware_lib;

import java.io.*;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by pisare_d on 12.12.13.
 */
public class Marshalling {

    public static String marshall(ArrayList<Object> objects) throws IOException {
        return encode(objects);
    }

    public static String marshall(Object object) throws IOException {
        ArrayList<Object> objects = new ArrayList<Object>();
        objects.add(object);
        return encode(objects);
    }

    private static String encode(ArrayList<Object> objects) throws IOException {
        ByteArrayOutputStream outputStream      = new ByteArrayOutputStream();
        ObjectOutputStream objectOutputStream   = new ObjectOutputStream(outputStream);
        objectOutputStream.writeObject(objects);
        return objectOutputStream.toString();
    }

    public static ArrayList<Object> unmarshall(String params) throws IOException, ClassNotFoundException {
        ArrayList<Object> result            = new ArrayList<Object>();
        ByteArrayInputStream inputStream    = new ByteArrayInputStream(params.getBytes());
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
}
