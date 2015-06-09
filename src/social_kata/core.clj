(ns social-kata.core
  (:require [schema.core :as s]))

(def state-of-world-schema
  {s/Str
   {:timeline [{:message s/Str
                :author s/Str}]
    (s/optional-key :mentions) #{s/Str}
    (s/optional-key :subscriptions) #{s/Str}}})

(defn publish
  [state user msg]
  (s/validate state-of-world-schema state)
  (update-in state [user] conj
             {:mentions (extract-mentions msg)
              :message  msg}))

(defn extract-mentions
  [message]
  (into #{} (map second (re-seq #"@(\w*)" message))))

(defn view
  [state user]
  (s/validate state-of-world-schema state)
  (get-in state [user :timeline] []))

(defn subscribe
  [state user subscription]
  (s/validate state-of-world-schema state)
  (update-in state [user :subscriptions] conj subscription))

(defn feed
  [state user]
  (s/validate state-of-world-schema state)
  (let [subs (get-in state [user :subscriptions])]
    (vec
     (for [subscribee subs
            {:keys [message author]} (view state subscribee)]
        {:author author :message message}))))
