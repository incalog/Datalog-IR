package language.util;

public class Pair<L, R> {

        private final L first;
        private final R second;


        public Pair(L first, R second) {
            this.first = first;
            this.second = second;
        }

        public L getFirst() {
            return first;
        }

        public R getSecond() {
            return second;
        }

}
