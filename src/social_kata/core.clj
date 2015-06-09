(ns social-kata.core)

(defn publish
  [state user msg]
  (update-in state [user] conj
             {:mentions (extract-mentions msg)
              :message  msg}))

(defn extract-mentions
  [message]
  (into #{} (map second (re-seq #"@(\w*)" message))))

(defn view
  [state user]
  (get-in state [user :timeline] []))

(defn subscribe
  [state user subscription]
  (update-in state [user :subscriptions] conj subscription))

(defn feed
  [state user]
  (let [subs (get-in state [user :subscriptions])]
    (vec
     (for [subscribee subs
            {:keys [message author]} (view state subscribee)]
        {:author author :message message}))))
