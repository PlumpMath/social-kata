(ns social-kata.core
  (:require [schema.core :as s]
            [clj-time.core :as t])
  (:import [org.joda.time DateTime]))

(def state-of-world-schema
  {s/Str
   {:timeline [{:message s/Str
                :author s/Str
                :timestamp org.joda.time.DateTime}]
    (s/optional-key :mentions) #{s/Str}
    (s/optional-key :subscriptions) #{s/Str}}})

(defn extract-mentions
  [message]
  (into #{} (map second (re-seq #"@(\w*)" message))))

(defn publish
  [state user msg]
  (s/validate state-of-world-schema state)
  (update-in state [user] conj
             {:mentions (extract-mentions msg)
              :message  msg}))
(defn view
  [state user]
  (s/validate state-of-world-schema state)
  (->>
   (get-in state [user :timeline] [])
   (sort-by :timestamp t/after?)))

(defn subscribe
  [state user subscription]
  (s/validate state-of-world-schema state)
  (update-in state [user :subscriptions] conj subscription))

(defn feed
  [state user]
  (s/validate state-of-world-schema state)
  (let [subs (get-in state [user :subscriptions])]
    (vec
     (sort-by :timestamp t/after?
      (for [subscribee subs
            {:keys [message author timestamp]} (view state subscribee)]
        {:author author :message message :timestamp timestamp})))))

(defn view-all
  "View all of a users timeline including all follows (timelines subscribed to)"
  [state user]
  (s/validate state-of-world-schema state)
  (->>
   (into (view state user)
         (feed state user))
   (sort-by :timestamp t/after?)
   vec))
