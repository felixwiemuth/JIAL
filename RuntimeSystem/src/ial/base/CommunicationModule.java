/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package ial.base;

import java.util.Set;

/**
 *
 * @author Felix Wiemuth
 */
public interface CommunicationModule {
    void sendMessage(Message m);
    
    int getNewID();
    
    /**
     * Get the IDs of the tasks of the given type.
     * @param name
     * @return 
     */
    Set<Integer> getGroup(String name);
    
    /**
     * Get the IDs of all registered tasks.
     * @return 
     */
    Set<Integer> getIDs();
    
}
