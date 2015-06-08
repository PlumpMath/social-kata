(ns social-kata.core)

(defn publish
  [state user msg]
  (update-in state [user] conj msg))

(defn view
  [state user]
  (get state user []))

(defn subscribe
  [state user subscription]
  (update-in state [user :subscriptions] conj subscription))
