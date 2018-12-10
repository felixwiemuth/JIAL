package ial.base;

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.function.Consumer;
import java.util.function.Predicate;

import java.util.ArrayList;

/**
 *
 * @author Felix Wiemuth
 */
public class Task {

    protected int $ID;
    protected Set<Integer> $ALL;

    /**
     * Represents an input-action pair.
     */
    protected class InputActionPair {

        public InputActionPair(Class<? extends Message> msgType, Predicate<Message> guard, Consumer<Message> action) {
            this.msgType = msgType;
            this.guard = guard;
            this.action = action;
        }

        Class<? extends Message> msgType;
        Predicate<Message> guard;
        Consumer<Message> action; // TODO need no return type

    }

    private CommunicationModule communicationModule;
    private final Set<Message> inputBuffer = new HashSet<>();
    protected Map<Class<? extends Message>, List<InputActionPair>> iaps = new HashMap<>();

    public void setCommunicationModule(CommunicationModule communicationModule) {
        this.communicationModule = communicationModule;
    }

    /**
     * Register the task with the communication module.
     */
    public void register() {
        $ID = communicationModule.register(this);
    }

    /**
     * Prepare the task for initialization. Must be called after
     * {@link #register()}.
     */
    public void prepare() {
        $ALL = communicationModule.getIDs();
    }

    protected void addIAP(InputActionPair iap) {
        if (!iaps.containsKey(iap.msgType)) {
            iaps.put(iap.msgType, new ArrayList<>());
        }
        iaps.get(iap.msgType).add(iap);
    }

    protected void addIAP(Class<? extends Message> msgType, Predicate<Message> guard, Consumer<Message> action) {
        addIAP(new InputActionPair(msgType, guard, action));
    }

    protected Set<Integer> getGroup(String name) {
        return communicationModule.getGroup(name);
    }

    public void init() {
    }

    public void addMessage(Message m) {
        inputBuffer.add(m);
    }

    /**
     * Let the task take a step (perform one action consuming one message) if
     * possible.
     *
     * @return true if the task took a step
     */
    public boolean step() {
        for (Message m : inputBuffer) {
            List<InputActionPair> iapsForMsgType = iaps.get(m.getClass());
            if (iapsForMsgType == null) {
                continue;
            }
            for (InputActionPair iap : iapsForMsgType) {
                if (iap.guard.test(m)) {
                    String s = getClass().getSimpleName() + "(" + getID() + ") processes " + m.getClass().getSimpleName();
                    if (m.getSrc() >= 0) {
                        s += " from " + m.getSrc();
                    }
                    System.out.println(s);
                    inputBuffer.remove(m);
                    iap.action.accept(m);
                    return true;
                }
            }
        }
        return false;
    }

    public int getID() {
        return $ID;
    }

    protected void send(Message m) {
        communicationModule.sendMessage(m);
    }
}
