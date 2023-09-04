(ns add-delete-tx
  (:require
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [fsm-test-check.core :as fsm]))

(def add-cmd
  (reify
    fsm/Command
    (precondition [_ state]
      (vector? (:people state)))

    (postcondition [_ state cmd]
      ;;add only valid if no other person with same id
      (->> (:people state)
           (filter #(= (:id %) (:id cmd)))
           seq
           nil?))

    (exec [_ state cmd]
      (update-in state [:people] (fn [people]
                                   (conj people
                                         (dissoc cmd :type)))))

    (generate [_ {:keys [people]}]
      (let [ids (into #{} (map :id) people)]
        (gen/fmap (partial zipmap [:type :name :id])
                  (gen/tuple (gen/return :add-cmd)
                             (gen/not-empty gen/string-alphanumeric)
                             (gen/such-that #(not (ids %))
                                            gen/small-integer)))))))

(def delete-cmd
  (reify
    fsm/Command
    (precondition [_ state]
      (seq (:people state)))

    (postcondition [_ state cmd]
      ;;delete only valid if existing person with id
      (->> (:people state)
           (filter #(= (:id %) (:id cmd)))
           seq))

    (exec [_ state cmd]
      (update-in state [:people] (fn [people]
                                   (vec (filter #(not= (:id %)
                                                       (:id cmd))
                                                people)))))

    (generate [_ state]
      (gen/fmap (partial zipmap [:type :id])
                (gen/tuple (gen/return :delete-cmd)
                           (gen/elements (mapv :id (:people state))))))))

;;-----------------------------------------------------
;;property definition

(defn apply-tx
  "Apply transactions fails when there are two delete commands"
  [tx-log]
  (->> tx-log
       (filter #(= :delete-cmd (:type %)))
       count
       (> 2)))

(def commands-consistent-apply
  (prop/for-all [tx-log (fsm/cmd-seq {:people []} {:add-cmd add-cmd :delete-cmd delete-cmd})]
                (true? (apply-tx tx-log))))

(comment
  (tc/quick-check 100 commands-consistent-apply)
  )
