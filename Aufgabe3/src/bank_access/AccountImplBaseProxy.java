package bank_access;

import mware_lib.Connection;
import mware_lib.Marshalling;
import mware_lib.Proxy;

import java.io.IOException;
import java.util.ArrayList;

/**
 * Created by pisare_d on 05.12.13.
 */
public class AccountImplBaseProxy extends AccountImplBase {
    protected String hostname;
    protected int port;
    protected String name;

    @Override
    public void transfer(double amount) throws OverdraftException {
        Proxy.invoke(hostname, port, name, "transfer", amount);
    }

    @Override
    public double getBalance() {
        Object result = Proxy.invoke(hostname, port, name, "getBalance", new ArrayList<Object>()).get(0);
        return (double) result;
    }


}
