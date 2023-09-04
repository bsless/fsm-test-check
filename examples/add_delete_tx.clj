(ns add-delete-tx
  (:require
   [clojure.test.check :as tc]
   [clojure.test.check.generators :as gen]
   [clojure.test.check.properties :as prop]
   [fsm-test-check.core :as fsm]))

(defmacro map-gen
  "hash map gen"
  [& kvs]
  (let [ks (vec (take-nth 2 kvs))
        vs (take-nth 2 (rest kvs))]
    `(gen/fmap #(zipmap ~ks %) (gen/tuple ~@vs))))

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
      (update state :people (fn [people]
                              (conj people
                                    (dissoc cmd :type)))))

    (generate [_ {:keys [people]}]
      (let [ids (into #{} (map :id) people)]
        (map-gen
         :type (gen/return :add-cmd)
         :name (gen/not-empty gen/string-alphanumeric)
         :id (gen/such-that #(not (ids %)) gen/small-integer))))))

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
      (update state :people (fn [people]
                              (filterv #(not= (:id %) (:id cmd)) people))))

    (generate [_ state]
      (map-gen
        :type (gen/return :delete-cmd)
        :id (gen/elements (mapv :id (:people state)))))))

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
