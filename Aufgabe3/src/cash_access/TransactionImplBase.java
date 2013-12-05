package cash_access;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:15

 */
public abstract class TransactionImplBase {
    public abstract void deposit(String accountId,double amount)    throws InvalidParamException;
    public abstract void withdraw(String accountId,double amount)   throws InvalidParamException,OverdraftException;
    public abstract double getBalance(String accountId)             throws InvalidParamException;
    public static TransactionImplBase narrowCast(Object gor) {
        return (TransactionImplBase) gor;
    }
}
