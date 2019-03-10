package ial.simulation;

import ial.base.Message;
import ial.base.Task;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 *
 * @author Felix Wiemuth
 */
public class SynchronousLocalCommunicationModule extends LocalCommunicationModule {
    private final Map<Integer, Set<Message>> messages = new HashMap<>();
    
    /**
     * Send the messages collected in the current round.
     */
    public void sendMessages() {
        messages.keySet().forEach((dest) -> {
            messages.get(dest).forEach((m) -> {
                tasks.get(dest).addMessage(m);
                Task srcTask = tasks.get(m.getSrc());
                System.out.println(srcTask.getClass().getSimpleName() + "(" + m.getSrc() + ") sends " + m.getClass().getSimpleName() + " to " + m.getDest().toString());
            });
        });
    }

    @Override
    public void sendMessage(Message m) {
        m.getDest().forEach((dest) -> {
            if (!messages.containsKey(dest)) {
                messages.put(dest, new HashSet<>());
            }
            Set<Message> messageSet = messages.get(dest);
            messageSet.add(m);
        });
        
    }
    
    
    
}
