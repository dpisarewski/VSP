package mware_lib;

import name_service.NameServiceServer;

import java.io.IOException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.HashMap;
import java.util.Map;
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
    public void rebind(Object servant, String name) throws IOException {
        logger.info("Binding object " + name + ": " + servant.toString());
        String response;
        objectBroker.putObject(name, servant);
        Connection connection = new Connection(hostname, port);
        response = connection.sendAndRead(Marshalling.encodeRebind(name, servant.getClass().getSuperclass().getCanonicalName(), Dispatcher.hostname, Dispatcher.getInstance().getPort()));
        logger.info("Received response: " + response);
    }

    @Override
    public Object resolve(String name) throws IOException, ClassNotFoundException {
        logger.info("Resolving object: " + name);
        Connection connection   = new Connection(hostname, port);
        String response         = connection.sendAndRead(Marshalling.encodeResolve(name));
        logger.info("Received response: " + response);

        Map<String, Object> objectValues    = Marshalling.unmarshall(response);
        String command                      = (String) objectValues.get("command");
        if(command.equals("RESULT")){
            String objectName           = (String) objectValues.get("name");
            String objectClassName      = (String) objectValues.get("classname");
            String objectHost           = (String) objectValues.get("hostname");
            Integer objectPort          = (Integer) objectValues.get("port");
            try {
                Constructor<?> constructor = Class.forName(objectClassName + "Proxy").getConstructor(new Class[]{String.class, String.class, int.class});
                return constructor.newInstance(objectName, objectHost, objectPort);
            } catch (NoSuchMethodException | ClassNotFoundException | InvocationTargetException | IllegalAccessException | InstantiationException e) {
                e.printStackTrace();
            }
        }
        return null;
    }
}
