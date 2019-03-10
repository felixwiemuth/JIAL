package ial.simulation;

import ial.base.Message;
import ial.base.Task;

/**
 *
 * @author Felix Wiemuth
 */
public class SynchronousSimulator extends Simulator {

    private final int rounds;

    /**
     *
     * @param rounds How many rounds to run after the initial round (0)
     */
    public SynchronousSimulator(int rounds) {
        com = new SynchronousLocalCommunicationModule();
        this.rounds = rounds;
    }

    /**
     * Run the simulation until no task can take a step anymore.
     */
    @Override
    public void run() {
        tasks.forEach(Task::register);
        tasks.forEach(Task::prepare);
        tasks.forEach(t -> t.addMessage(new Message.init()));
        System.out.println("Initialization round");
        runTasksUntilNoMoreActivity();
        for (int round = 1; round < rounds; round++) {
            final int r = round;
            System.out.println("Round " + r);
            tasks.forEach(t -> t.addMessage(new Message.tic(r)));
            //System.out.println("Deliver messages...");
            ((SynchronousLocalCommunicationModule) com).sendMessages();
            //System.out.println("Process messages....");
            runTasksUntilNoMoreActivity();
        }
    }
}
