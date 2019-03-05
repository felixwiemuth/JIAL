package ial.simulation;

import ial.base.CommunicationModule;
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
public class LocalCommunicationModule implements CommunicationModule {

    private int nextID = 0;
    private final Map<Integer, Task> tasks = new HashMap<>();
    private final Map<String, Set<Integer>> groups = new HashMap<>();

    @Override
    public int register(Task task) {
        int id = nextID;
        nextID++;
        tasks.put(id, task);
        String name = task.getClass().getSimpleName();
        if (groups.get(name) == null) {
            groups.put(name, new HashSet<>());
        }
        groups.get(name).add(id);
        return id;
    }

    @Override
    public void sendMessage(Message m) {
        m.getDest().forEach((dest) -> {
            tasks.get(dest).addMessage(m);
        });
        Task srcTask = tasks.get(m.getSrc());
        System.out.println(srcTask.getClass().getSimpleName() + "(" + m.getSrc() + ") sends " + m.getClass().getSimpleName() + " to " + m.getDest().toString());
    }

    @Override
    public Set<Integer> getGroup(String name) {
        return groups.get(name);
    }

    @Override
    public Set<Integer> getIDs() {
        return tasks.keySet();
    }

}
