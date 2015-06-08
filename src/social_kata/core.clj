(ns social-kata.core)

(defn publish
  [state user msg]
  (update-in state [user] conj msg))

(defn view
  [state user]
  (get-in state [user :timeline] []))

(defn subscribe
  [state user subscription]
  (update-in state [user :subscriptions] conj subscription))

(defn feed
  [state user]
  (let [subs (get-in state [user :subscriptions])]
    (for [author subs
          message (view state author)]
      {:author author :message message})))
