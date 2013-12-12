package bank_access;

import mware_lib.Proxy;

import java.util.ArrayList;

/**
 * Created by pisare_d on 05.12.13.
 */
public class ManagerImplBaseProxy extends ManagerImplBase {
    protected String hostname;
    protected int port;
    protected String name;

    @Override
    public String createAccount(String owner, String branch) {
        ArrayList<Object> params = new ArrayList<Object>();
        params.add(owner);
        params.add(branch);
        Object result = Proxy.invoke(hostname, port, name, "createAccount", params).get(0);
        return (String) result;
    }
}
