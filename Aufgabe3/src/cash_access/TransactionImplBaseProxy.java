package cash_access;

import mware_lib.Proxy;

import java.util.ArrayList;
import java.util.List;

/**
 * Created by pisare_d on 05.12.13.
 */
public class TransactionImplBaseProxy extends TransactionImplBase {
    protected String hostname;
    protected int port;
    protected String name;

    public TransactionImplBaseProxy(String name, String hostname, int port){
        this.name       = name;
        this.hostname   = hostname;
        this.port       = port;
    }

    @Override
    public void deposit(String accountId, double amount) throws InvalidParamException {
        List<Object> params = new ArrayList<Object>();
        params.add(accountId);
        params.add(amount);
        Proxy.invoke(hostname, port, name, "deposit", params);
    }

    @Override
    public void withdraw(String accountId, double amount) throws InvalidParamException, OverdraftException {
        List<Object> params = new ArrayList<Object>();
        params.add(accountId);
        params.add(amount);
        Proxy.invoke(hostname, port, name, "withdraw", params);
    }

    @Override
    public double getBalance(String accountId) throws InvalidParamException {
        List<Object> params = new ArrayList<Object>();
        params.add(accountId);
        List<Object> result = (List<Object>) Proxy.invoke(hostname, port, name, "getBalance", params).get("params");
        return (double) result.get(0);
    }
}
