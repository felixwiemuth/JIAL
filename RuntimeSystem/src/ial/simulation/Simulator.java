package ial.simulation;

import ial.base.CommunicationModule;
import ial.base.Message;
import ial.base.Task;
import java.util.ArrayList;
import java.util.List;

/**
 *
 * @author Felix Wiemuth
 */
public class Simulator {
    private final CommunicationModule com = new LocalCommunicationModule();
    private final List<Task> tasks = new ArrayList<>();
    
    public void addTask(Task task) {
        tasks.add(task);
        task.setCommunicationModule(com);
    }
    
    public void addTasks(Task... add) {
        for (Task task : add) {
            addTask(task);
        }
    }
    
    /**
     * Run the simulation until no task can take a step anymore.
     */
    public void run() {
        tasks.forEach(Task::register);
        tasks.forEach(Task::prepare);
        tasks.forEach(t -> t.addMessage(new Message.init()));
        boolean run = true;
        while (run) {
            run = false;
            for (Task task : tasks) {
                if (task.step()) {
                    run = true;
                }
            }
        }
        
    }
}
