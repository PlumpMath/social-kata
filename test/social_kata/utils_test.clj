(ns social-kata.utils-test
  (:require [social-kata.utils :refer :all]
            [clojure.test :refer :all]
            [social-kata.core :as c]
            [matcha :as m]
            [clj-time.core :as t]))

(deftest test-timestamp-in-timeline-parsed-to-string
  (testing "Parse timestamps in timeline to string"
    (let [timestamp (t/date-time 2015 01 01 12)
          timeline  [(c/map->TimelineEntry
                      {:author "Chris"
                       :message "test msg"
                       :timestamp timestamp})]]
      (m/is (m/= "01-01-2015 12:00:00 PM")
            (get (first (timeline-timestamp-to-str timeline)) :timestamp)))))
