(ns social-kata.utils
  (:require [clj-time.format :as tf]
            [compojure.core :refer :all]
            [ring.util.response :refer [response]]
            [social-kata
             [core :refer :all]]
            [compojure.core :refer :all]
            [ring.util.response :refer [response]]
            [social-kata
             [core :refer :all]]))

(defn- format-timestamp
  "Format timestamp"
  [timestamp]
  (tf/unparse (tf/formatter "dd-MM-yyyy hh:mm:ss a") timestamp))

(defn timeline-timestamp-to-str
  "Parse all timestamps in timeline from clj-time to string."
  [timeline]
  (map #(update-in % [:timestamp] format-timestamp) timeline))
