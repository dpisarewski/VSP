package mware_lib;

/**
 * Created with IntelliJ IDEA.
 * User: pisare_d
 * Date: 28.11.13
 * Time: 15:36
 */
public class Skeleton extends Thread{

    private Connection connection;

    public Skeleton(Connection connection){
        this.connection = connection;
    }

    public void run(){
        String request = connection.readAll();
        if (request != null){
            //request.split("#")[0];
        }


    }


}
