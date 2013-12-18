package bank_access;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 14:14

 */
public abstract class ManagerImplBase {

    public abstract String createAccount(String owner,String branch) throws Exception;
    public static ManagerImplBase narrowCast(Object gor) {
        return (ManagerImplBase) gor;
    }
}
