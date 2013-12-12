package mware_lib;

import java.util.HashMap;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:12

 */
public class ObjectBroker {

    private NameService nameService;
    private static ObjectBroker instance;
    private HashMap<String, Object> registry;

    private ObjectBroker(String hostname, int port){
        nameService = new NameServiceImpl(this, hostname, port);
        registry    = new HashMap<String, Object>();
        Dispatcher.getInstance().start();
    }

    /**
     * @return an Implementation for a local NameService
     */
    public NameService getNameService() {
        return nameService;
    }
    /**
     * shuts down the process, the OjectBroker is running in
     * terminates process
     */
    public void shutDown() {
        System.exit(0);
    }
    /**
     * Initializes the ObjectBroker / creates the local NameService
     * @param serviceName
     * hostname or IP of Nameservice
     * @param port
     * port NameService is listening at
     * @return an ObjectBroker Interface to Nameservice
     */
    public synchronized static ObjectBroker init(String serviceName, int port) {
        if(instance == null){
            instance = new ObjectBroker(serviceName, port);
        }
        return instance;
    }

    public synchronized static ObjectBroker getInstance() {
        return instance;
    }

    public synchronized void putObject(String name, Object object){
        registry.put(name, object);
    }

    public synchronized Object getObject(String name){
        return registry.get(name);
    }



}
