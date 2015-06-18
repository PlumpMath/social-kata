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

(s/defrecord TimelineEntry
    [message :- s/Str
     author :- s/Str
     timestamp :- org.joda.time.DateTime])

(s/defrecord User
  [username :- s/Str
   timeline :- [TimelineEntry]
   mentions :- clojure.lang.APersistentSet
   subscriptions :- clojure.lang.APersistentSet])

(defn ^:always-validate new-user
  "Constructor to create a User record"
  ([username]
   (->User username [] #{} #{}))
  ([username
    timeline]
   (new-user username timeline #{} #{}))
  ([username
    timeline
    mentions]
   (new-user username timeline mentions #{}))
  ([username
    timeline
    mentions
    subscriptions]
   (->User username (vector timeline)  (set mentions) (set subscriptions))))

(defn map->new-user
  "Constructor for a new User from a map. Takes a map of username, a timeline, an optional set of mentions and an optional set of subscriptions"
  [{:keys [username timeline mentions subscriptions] :or {mentions #{} subscriptions #{}}}]
  (->User username (into [] timeline)  (into #{} mentions) (into #{} subscriptions)))

(defn new-world [user-col]
  "Constructor for a 'world' of social media! Takes a collection of users."
  (reduce #(assoc %1 (:username %2) %2) {} user-col))

(defn extract-mentions
  "Extract the username of any user mentioned in message text.
  Uses '@' to indicate a user."
  [message]
  (into #{} (map second (re-seq #"@(\w*)" message))))

(defn publish
  "Publish a message to the specified users timeline."
  [state user msg]
  (s/validate state-of-world-schema state)
  (update-in state [user] conj
             {:mentions (extract-mentions msg)
              :message  msg}))

(defn- update-user
  [user username msg]
  (let [mentions (extract-mentions msg)]
   (if (nil? user)
      (new-user username (->TimelineEntry msg username (t/now)) mentions)
      (->
       (update-in user [:timeline] #(conj % (->TimelineEntry msg username (t/now))))
       (update-in [:mentions] into mentions)
       ))))

(s/defn ^:always-validate publish-rec :- {s/Str User}
  [state :- {s/Str User}
   user  :- s/Str
   msg   :- s/Str]
  (-> state
      (update-in [user]
                 #(update-user % user msg))))

(defn view
  [state user]
  (s/validate state-of-world-schema state)
  (->>
   (get-in state [user :timeline] [])
   (sort-by :timestamp t/after?)))

(s/defn view-rec :- [TimelineEntry]
  [state :- {s/Str User}
   user  :- s/Str]
  (->>
   (get-in state [user :timeline] [])
   (sort-by :timestamp t/after?)))

(defn subscribe
  [state user subscription]
  (s/validate state-of-world-schema state)
  (update-in state [user :subscriptions] conj subscription))

(s/defn ^:always-validate subscribe-rec :- {s/Str User}
  [state        :- {s/Str User}
   username     :- s/Str
   subscription :- s/Str]
  (update-in state [username :subscriptions] conj subscription))

(defn feed
  [state user]
  (s/validate state-of-world-schema state)
  (let [subs (get-in state [user :subscriptions])]
    (vec
     (sort-by :timestamp t/after?
              (for [subscribee subs
                    {:keys [message author timestamp]} (view state subscribee)]
                {:author author :message message :timestamp timestamp})))))

(defn feed-rec
  [state username]
  (let [subs (get-in state [username :subscriptions])]
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


(comment
  (new-user "Chris" [(->TimelineEntry "message" "Chris" (t/now))])

  (new-user {:username "Chris" :timeline [(->TimelineEntry "Chris" "message" (t/now))] :subscriptions #{"Fred"}})
  (defn my-range [x y] (loop [acc [x] n x]
                         (let [n (inc n)]
                           (if (= n y)
                             acc
                             (recur (conj acc n) n)))))

  (def world1 (new-world [(new-user "Chris"
                                  (->TimelineEntry "message" "Chris" (t/now))
                                  ["Fred" "Jim" "Fred"])
                        (new-user "Fred"
                                  (->TimelineEntry "message from Fred" "Fred" (t/now)))]))
  world1
  (publish-rec (new-world []) "Chris" "Hello from Starbucks")


  )
