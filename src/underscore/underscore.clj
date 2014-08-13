(ns underscore.underscore
  (:require [n01se.syntax :refer :all]
            [n01se.seqex :refer [cap recap]]))

(defterminal binding-form some?)
(defterminal <- #(= "<-" (str %)))
(defterminal expr (constantly true))

(defrule binding-rule
  (recap (alt (cat (cap binding-form) (alt <- :=) (cap expr))
              (list-form
               (cat (alt 'bind 'deflet :=) (cap binding-form) (cap expr))))
         (fn [[bf] [expr]] {:binding bf :expr expr :is-binding-rule? true})))

(defn- to-lets [stxs]
  (cond (empty? stxs) nil
        (:is-binding-rule? (ffirst stxs)) 
        `(let ~(vec (apply concat (map (fn [{:keys [binding expr]}]
                                         [binding expr])
                                       (first stxs))))
           ~(to-lets (rest stxs)))
        (= (count stxs) 1) `(do ~@(first stxs))
        :else `(do ~@(first stxs)
                   ~(to-lets (rest stxs)))))

(defrule underscore
  (recap (rep* (alt binding-rule (cap expr first)))
         (fn [& body]
           (->> body
                (partition-by :is-binding-rule?)
                to-lets))))

(defsyntax _ underscore)
(defsyntax do-with-lets underscore)
