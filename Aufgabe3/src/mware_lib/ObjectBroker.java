package mware_lib;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:12

 */
public class ObjectBroker {

    NameService nameService;

    public ObjectBroker(String hostname, int port){
        nameService = new NameServiceImpl(hostname, port);
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
    public static ObjectBroker init(String serviceName, int port) {
        return new ObjectBroker(serviceName, port);
    }

}
