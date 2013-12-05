package cash_access;

/**
 * Created by pisare_d on 05.12.13.
 */
public class TransactionImplBaseProxy extends TransactionImplBase {
    @Override
    public void deposit(String accountId, double amount) throws InvalidParamException {

    }

    @Override
    public void withdraw(String accountId, double amount) throws InvalidParamException, OverdraftException {

    }

    @Override
    public double getBalance(String accountId) throws InvalidParamException {
        return 0;
    }
}
