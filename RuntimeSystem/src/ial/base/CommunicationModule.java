package ial.base;

import java.util.Set;

/**
 *
 * @author Felix Wiemuth
 */
public interface CommunicationModule {

    void sendMessage(Message m);

    /**
     * Register a task with the communication module. Returns the ID to be used
     * to address the task.
     *
     * @param task
     * @return
     */
    int register(Task task);

    /**
     * Get the IDs of the tasks of the given type.
     *
     * @param name
     * @return
     */
    Set<Integer> getGroup(String name);

    /**
     * Get the IDs of all registered tasks.
     *
     * @return
     */
    Set<Integer> getIDs();

}
