package ial.example.twophasecommit;

import ial.example.twophasecommit.Vote;
import ial.example.twophasecommit.Transaction;

import java.util.Set;
import ial.base.Task;
import ial.base.Message;

public class Coordinator extends Task {

    private Set<Integer> $Coordinator;
    private Set<Integer> $Participant;

    public void prepare() {
        super.prepare();
        $Coordinator = getGroup("Coordinator");
        $Participant = getGroup("Participant");
        addIAP(Message.init.class, m -> init$Guard(m), m -> init$Action(m));
        addIAP(M.vote.class, m -> vote$Guard(m), m -> vote$Action(m));
    }

    private Transaction t = new Transaction();
    private Vote v = Vote.COMMIT;
    private int count = 0;

    public boolean init$Guard(Message _m) {
        int $src = _m.getSrc();

        return true;
    }

    public void init$Action(Message _m) {
        int $src = _m.getSrc();

        if (v == Vote.ABORT) {

            {
                Message m_ = new M.abort();
                m_.setSrc($ID);
                m_.setDest($Participant);
                send(m_);
            }

            t.abort();
        } else {

            {
                Message m_ = new M.vote_request();
                m_.setSrc($ID);
                m_.setDest($Participant);
                send(m_);
            }

        }

    }

    public boolean vote$Guard(Message _m) {
        int $src = _m.getSrc();
        Vote x = ((M.vote) _m).p1;
        return true;
    }

    public void vote$Action(Message _m) {
        int $src = _m.getSrc();
        Vote x = ((M.vote) _m).p1;

        if (x == Vote.ABORT) {

            {
                Message m_ = new M.abort();
                m_.setSrc($ID);
                m_.setDest($Participant);
                send(m_);
            }

            t.abort();
        } else {
            count++;
            if (count == $Participant.size()) {

                {
                    Message m_ = new M.commit();
                    m_.setSrc($ID);
                    m_.setDest($Participant);
                    send(m_);
                }

                t.commit();
            }
        }

    }

}
