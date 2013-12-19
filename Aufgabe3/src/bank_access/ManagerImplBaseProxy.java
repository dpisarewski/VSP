package bank_access;

import mware_lib.Proxy;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by pisare_d on 05.12.13.
 */
public class ManagerImplBaseProxy extends ManagerImplBase {
    protected String hostname;
    protected int port;
    protected String name;

    public ManagerImplBaseProxy(String name, String hostname, int port){
        this.name       = name;
        this.hostname   = hostname;
        this.port       = port;
    }

    @Override
    public String createAccount(String owner, String branch) throws Exception {
        List<Object> params = new ArrayList<Object>();
        params.add(owner);
        params.add(branch);
        List<Object> result = (List<Object>) Proxy.invoke(hostname, port, name, "createAccount", params, true).get("params");
        return (String) result.get(0);
    }
}
