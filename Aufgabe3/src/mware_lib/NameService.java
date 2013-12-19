package mware_lib;

import java.io.IOException;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:13

 */
public abstract class NameService {
    /**
     * Registers a remote object / service for name
     * @param servant object, processing remote methods
     * @param name a global unique name of the object / service
     */
    public abstract void rebind(Object servant, String name) throws IOException;
    /**
     * Resolves name to a generic object reference
     * @param name
     * @return a generic object reference
     */
    public abstract Object resolve(String name) throws IOException, ClassNotFoundException;
}
