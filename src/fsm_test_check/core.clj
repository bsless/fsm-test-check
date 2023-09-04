(ns fsm-test-check.core
  (:require
   [clojure.test.check.generators :as gen]
   [clojure.test.check.rose-tree :as rose]))

(defprotocol Command
  (precondition [this state] "Returns true if command can be applied in current system state")
  (postcondition [this state cmd] "Returns true if cmd can be applied on specified state")
  (exec [this state cmd] "Applies command in the specified system state, returns new state")
  (generate [this state] "Generates command given the current system state, returns command"))


(defn valid-sequence?
  [commands state-seq cmd-seq sub-seq-idxs]
  (when (seq sub-seq-idxs)
    (map? (reduce (fn [curr-state ^long state-idx]
                       (let [cmd (nth cmd-seq state-idx)
                             command (get commands (:type cmd))]
                         (if (postcondition command curr-state cmd)
                           (exec command curr-state cmd)
                           (reduced false))))
                     (first state-seq)
                     sub-seq-idxs))))

(defn shrink-sequence
  [cmd-seq state-seq commands]
  (letfn [(shrink-subseq [idxs]
            (when (seq idxs)
              (rose/make-rose
               (map #(nth cmd-seq %) idxs)
               (into
                []
                (comp
                 (map (fn [^long i] (filterv (fn [^long j] (if (= i j) false true)) idxs)))
                 (filter (partial valid-sequence? commands state-seq cmd-seq))
                 (map shrink-subseq))
                idxs))))]
    (shrink-subseq (range 0 (count cmd-seq)))))

(defn cmd-seq-helper
  [state commands size]
  (gen/bind (gen/one-of (into []
                              (comp
                               (map second)
                               (filter #(precondition % state))
                               (map #(generate % state)))
                              commands))
            (fn [cmd]
              (if (zero? size)
                (gen/return [[cmd state]])
                (gen/fmap
                 (fn [xs] (cons [cmd state] xs))
                 (cmd-seq-helper (exec (get commands (:type cmd)) state cmd)
                                 commands
                                 (dec size)))))))

(defn cmd-seq
  [state commands]
  (gen/bind (gen/choose 0 5)
            (fn [num-elements]
              (gen/bind (cmd-seq-helper state commands num-elements)
                        (fn [cmd-seq]
                          (let [shrinked (shrink-sequence (mapv first cmd-seq)
                                                          (mapv second cmd-seq)
                                                          commands)]
                            (gen/gen-pure shrinked)))))))
