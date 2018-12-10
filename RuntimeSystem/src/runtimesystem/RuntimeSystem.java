package runtimesystem;

import example.Coordinator;
import example.Participant;
import ial.simulation.Simulator;
import ial.base.Task;

public class RuntimeSystem {

    public static void main(String[] args) {
        // Test the simulator with the two-phase commit protocol
        Simulator sim = new Simulator();
        Task c = new Coordinator();
        Task p1 = new Participant();
        Task p2 = new Participant();
        sim.addTasks(c, p1, p2);
        sim.run();
    }
    
}
