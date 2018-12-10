package ial.base;

import java.util.HashSet;
import java.util.Set;

public class Message {

    /**
     * The initial message to be sent to all tasks on initialization of an
     * algorithm.
     */
    public static class init extends Message {
    };

    private int src;
    private Set<Integer> dest;

    public int getSrc() {
        return src;
    }

    public void setSrc(int src) {
        this.src = src;
    }

    public Set<Integer> getDest() {
        return dest;
    }

    public void setDest(Set<Integer> dest) {
        this.dest = dest;
    }

    public void setDest(int dest) {
        this.dest = new HashSet<>();
        this.dest.add(dest);
    }
}
