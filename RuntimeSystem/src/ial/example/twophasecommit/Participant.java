package ial.example.twophasecommit;

import ial.example.twophasecommit.Vote;
import ial.example.twophasecommit.Transaction;

import java.util.Set;
import ial.base.Task;
import ial.base.Message;

public class Participant extends Task {

    private Set<Integer> $Coordinator;
    private Set<Integer> $Participant;

    public void prepare() {
        super.prepare();
        $Coordinator = getGroup("Coordinator");
        $Participant = getGroup("Participant");
        addIAP(M.vote_request.class, m -> vote_request$Guard(m), m -> vote_request$Action(m));
        addIAP(M.abort.class, m -> abort$Guard(m), m -> abort$Action(m));
        addIAP(M.commit.class, m -> commit$Guard(m), m -> commit$Action(m));
    }

    private Transaction t = new Transaction();
    private Vote v = Vote.COMMIT;

    public boolean vote_request$Guard(Message _m) {
        int $src = _m.getSrc();

        return true;
    }

    public void vote_request$Action(Message _m) {
        int $src = _m.getSrc();

        {
            Message m_ = new M.vote(v);
            m_.setSrc($ID);
            m_.setDest($src);
            send(m_);
        }

    }

    public boolean abort$Guard(Message _m) {
        int $src = _m.getSrc();

        return true;
    }

    public void abort$Action(Message _m) {
        int $src = _m.getSrc();

        t.abort();

    }

    public boolean commit$Guard(Message _m) {
        int $src = _m.getSrc();

        return true;
    }

    public void commit$Action(Message _m) {
        int $src = _m.getSrc();

        t.commit();

    }

}
