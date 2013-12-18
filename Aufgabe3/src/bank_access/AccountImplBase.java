package bank_access;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:14

 */
public abstract class AccountImplBase {
    public abstract void transfer(double amount) throws Exception;
    public abstract double getBalance() throws Exception;
    public static AccountImplBase narrowCast(Object gor) {
        return (AccountImplBase) gor;
    }
}
