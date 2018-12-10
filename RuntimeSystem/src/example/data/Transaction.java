package example.data;

/**
 *
 * @author Felix Wiemuth
 */
public class Transaction {
    private boolean result;

    public boolean getResult() {
        return result;
    }
    
    public void abort() {
        result = false;
        System.out.println("Transaction aborted");
    }
    
    public void commit() {
        result = true;
        System.out.println("Transaction committed");
    }
    
}
