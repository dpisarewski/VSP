package name_service;

import mware_lib.*;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:46
 */
public class NameServiceImpl extends NameService{

    String hostname;
    int port;

    public NameServiceImpl(String hostname, int port){
        this.hostname   = hostname;
        this.port       = port;
    }

    @Override
    public void rebind(Object servant, String name) {

    }

    @Override
    public Object resolve(String name) {
        return null;
    }
}
