(ns social-kata.handler-test
  (:require [social-kata.handler :refer :all]
            [clojure.test :refer :all]
            [matcha :as m]
            [ring.mock.request :refer :all]))

(deftest test-get-root
  (m/is (m/= "Testing...123") (:body (app (request :get "/")))))

(deftest test-get-timeline
  (testing "Get timeline for 'Chris' with two messages"
    (m/run-match (m/has-count 2) (:body (app (request :get "/api/timeline/Chris")))) ))
