package mware_lib;

import name_service.NameServiceServer;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:46
 */
public class NameServiceImpl extends NameService {

    String hostname;
    int port;
    ObjectBroker objectBroker;

    public NameServiceImpl(ObjectBroker objectBroker, String hostname, int port){
        this.hostname       = hostname;
        this.port           = port;
        this.objectBroker   = objectBroker;
    }

    @Override
    public void rebind(Object servant, String name) {
        String response;
        objectBroker.putObject(name, servant);
        Connection connection = new Connection(hostname, port);
        response = connection.send(NameServiceServer.REBIND + "#" + name + "#" + servant.getClass().getCanonicalName() + "#" + Dispatcher.hostname + "#" + Dispatcher.PORT);
    }

    @Override
    public Object resolve(String name) {
        String response;
        Connection connection = new Connection(hostname, port);
        response = connection.send(NameServiceServer.RESOLVE + "#" + name);
        if(response.split("#")[0].equals("OK")){
            String objectName   = response.split("#")[1];
            String objectClasse = response.split("#")[2];
            String objectHost   = response.split("#")[3];
            String objectPort   = response.split("#")[4];
            try {
                Constructor<?> constructor = Class.forName(objectName + "Proxy").getConstructor();
                return constructor.newInstance(objectName, objectClasse, objectHost, objectPort);
            } catch (NoSuchMethodException | ClassNotFoundException | InvocationTargetException | IllegalAccessException | InstantiationException e) {
                e.printStackTrace();
            }
        }
        return null;
    }
}
