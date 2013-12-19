package bank_access;

import mware_lib.Connection;
import mware_lib.Marshalling;
import mware_lib.Proxy;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by pisare_d on 05.12.13.
 */
public class AccountImplBaseProxy extends AccountImplBase {
    protected String hostname;
    protected int port;
    protected String name;

    public AccountImplBaseProxy(String name, String hostname, int port){
        this.name       = name;
        this.hostname   = hostname;
        this.port       = port;
    }

    @Override
    public void transfer(double amount) throws Exception {
        Proxy.invoke(hostname, port, name, "transfer", amount, false);
    }

    @Override
    public double getBalance() throws Exception {
        List<Object> result = (List<Object>) Proxy.invoke(hostname, port, name, "getBalance", null, true).get("params");
        return (double) result.get(0);
    }


}
