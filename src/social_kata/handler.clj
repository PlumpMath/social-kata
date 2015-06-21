(ns social-kata.handler
  (:require [clj-time.core :as t]
            [compojure.core :refer :all]
            [ring.middleware
             [defaults :refer [api-defaults wrap-defaults]]
             [json :refer [wrap-json-response]]]
            [ring.util.response :refer [response]]
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

(defroutes api-routes
  (GET "/" [] "Testing...123")
  (GET "/api/timeline/:user" [user]
       (response (timeline-timestamp-to-str (view-rec @world user)))))

(def app
  (wrap-json-response (wrap-defaults api-routes api-defaults)))
