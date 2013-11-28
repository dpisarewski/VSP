package bank_access;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:14

 */
public abstract class AccountImplBase {
    public abstract void transfer(double amount) throws OverdraftException;
    public abstract double getBalance();
    public static AccountImplBase narrowCast(Object gor) {
        return (AccountImplBase) gor;
    }
}
