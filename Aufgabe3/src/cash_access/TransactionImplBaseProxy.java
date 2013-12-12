package cash_access;

import mware_lib.Proxy;

import java.util.ArrayList;

/**
 * Created by pisare_d on 05.12.13.
 */
public class TransactionImplBaseProxy extends TransactionImplBase {
    protected String hostname;
    protected int port;
    protected String name;

    @Override
    public void deposit(String accountId, double amount) throws InvalidParamException {
        ArrayList<Object> params = new ArrayList<Object>();
        params.add(accountId);
        params.add(amount);
        Proxy.invoke(hostname, port, name, "deposit", params);
    }

    @Override
    public void withdraw(String accountId, double amount) throws InvalidParamException, OverdraftException {
        ArrayList<Object> params = new ArrayList<Object>();
        params.add(accountId);
        params.add(amount);
        Proxy.invoke(hostname, port, name, "withdraw", params);
    }

    @Override
    public double getBalance(String accountId) throws InvalidParamException {
        ArrayList<Object> params = new ArrayList<Object>();
        params.add(accountId);
        Object result = Proxy.invoke(hostname, port, name, "getBalance", params).get(0);
        return (double) result;
    }
}
