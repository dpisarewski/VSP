package bank_access;

/**
 * Created by pisare_d on 05.12.13.
 */
public class AccountImplBaseProxy extends AccountImplBase {
    @Override
    public void transfer(double amount) throws OverdraftException {

    }

    @Override
    public double getBalance() {
        return 0;
    }
}
