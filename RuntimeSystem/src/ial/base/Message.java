package ial.base;

import java.util.HashSet;
import java.util.Set;

public class Message {
    
    private int src;
    private Set<Integer> dest;

    public Message() {
        this.src = src;
        this.dest = dest;
    }

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
