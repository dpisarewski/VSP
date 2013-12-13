package mware_lib;

import name_service.NameServiceServer;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.logging.Logger;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:46
 */
public class NameServiceImpl extends NameService {

    private static final Logger logger = Logger.getLogger( NameServiceImpl.class.getName() );

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
        logger.info("Binding object " + name + ": " + servant.toString());
        String response;
        objectBroker.putObject(name, servant);
        Connection connection = new Connection(hostname, port);
        response = connection.sendAndRead(NameServiceServer.REBIND + "#" + name + "#" + servant.getClass().getSuperclass().getCanonicalName() + ";" + Dispatcher.hostname + ";" + Dispatcher.getInstance().getPort());
        logger.info("Received response: " + response);
    }

    @Override
    public Object resolve(String name) {
        logger.info("Resolving object: " + name);
        String response;
        Connection connection = new Connection(hostname, port);
        response = connection.sendAndRead(NameServiceServer.RESOLVE + "#" + name);
        logger.info("Received response: " + response);
        if(response.split("#")[0].equals("OK")){
            String objectName   = response.split("#")[1];
            String params       = response.split("#")[2];
            String objectClasse = params.split(";")[0];
            String objectHost   = params.split(";")[1];
            Integer objectPort  = Integer.valueOf(params.split(";")[2]);
            try {
                Constructor<?> constructor = Class.forName(objectClasse + "Proxy").getConstructor(new Class[]{String.class, String.class, int.class});
                return constructor.newInstance(objectName, objectHost, objectPort);
            } catch (NoSuchMethodException | ClassNotFoundException | InvocationTargetException | IllegalAccessException | InstantiationException e) {
                e.printStackTrace();
            }
        }
        return null;
    }
}
