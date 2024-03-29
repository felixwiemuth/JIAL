package ial.example.twophasecommit;

import ial.base.Message;
import ial.example.twophasecommit.Vote;

public class M {
    public static class init extends Message {}

    public static class abort extends Message {}

    public static class commit extends Message {}

    public static class vote_request extends Message {}

    public static class vote extends Message {
        public final Vote p1;

        public vote(Vote v) {
            this.p1 = v;
        }
    }
}
