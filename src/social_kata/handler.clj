(ns social-kata.handler
  (:require [clj-time.core :as t]
            [clojure.tools.logging :as log]
            [compojure.core :refer :all]
            [ring.middleware
             [defaults :refer [api-defaults wrap-defaults]]
             [json :refer [wrap-json-params wrap-json-response]]]
            [ring.util.response :refer [created response]]
            [social-kata
             [core :refer :all]
             [utils :refer [timeline-timestamp-to-str]]]))

(def world (atom (new-world
                  [(map->new-user
                    {:username "Chris"
                     :timeline
                     [(map->TimelineEntry
                       {:author "Chris"
                        :message "First message"
                        :timestamp (t/minus (t/now) (t/seconds 5))})
                      (map->TimelineEntry
                       {:author "Chris"
                        :message "Second message"
                        :timestamp (t/now)})]})])))

(defn- get-timeline
  [user]
  (timeline-timestamp-to-str (view-rec @world user)))

(defn- subscribe-to-timeline
  [user subs]
  (log/debug "About to subscribe to subs = " subs)
  (doseq [subscribee subs]
    (log/debug "Got subscribee=" subscribee)
    (swap! world subscribe-rec user subscribee))
  (created (str "/api/" user "/subscribe")
           {:subs (show-subscriptions @world user)}))

(defn- post-message
  [message user]
  (log/debug "Posted a new message '" message "' for user: " user)
  (created (str "/api/" user "/publish")
           (do
             (swap! world publish-rec user message)
             (get-timeline user))))

(defroutes api-routes
  (GET "/" [] "Testing...123")
  (GET "/api/:user/timeline" [user]
       (response (get-timeline user)))
  (POST "/api/:user/subscribe" [user subs]
        (subscribe-to-timeline user subs))
  (POST "/api/:user/publish" [user message]
        (post-message message user)))

(def app
  (wrap-json-params (wrap-json-response (wrap-defaults api-routes api-defaults))))
