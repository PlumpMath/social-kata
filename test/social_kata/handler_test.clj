(ns social-kata.handler-test
  (:require [social-kata.handler :refer :all]
            [clojure.test :refer :all]
            [matcha :as m]
            [ring.mock.request :as mock]
            [clj-time
             [format :as tf]
             [core :as t]]))

(deftest test-get-root
  (m/is (m/= "Testing...123") (:body (app (mock/request :get "/")))))

(deftest test-get-timeline
  (let [timeline-response (app (mock/request :get "/api/Chris/timeline"))]
    (testing "get timeline for 'Chris' check first message as expected"
      (m/run-match (m/contains-string "\"message\":\"First message\"")
                   (:body timeline-response)))
    (testing "get timeline for 'Chris' check first author as expected"
      (m/run-match (m/contains-string "\"author\":\"Chris\"")
                   (:body timeline-response)))
    (testing "get timeline for 'Chris' check second message as expected"
      (m/run-match (m/contains-string "\"message\":\"Second message\"")
                   (:body timeline-response)))))

(deftest test-follow
  (testing "Post a follow request for Chris to follow Alice"
    (let [follow-response (app (mock/content-type
                                (mock/request :post "/api/Chris/subscribe"
                                              "{\"subs\":[\"Alice\"]}")
                                "application/json"))]
     (m/is (m/= "{\"subs\":[\"Alice\"]}")
           (:body follow-response))
     (m/is (m/= 201) (:status follow-response))
     (m/is (m/contains-string "/api/Chris/subscribe")
           (get-in follow-response [:headers "Location"])))))

(deftest test-post-new-message
  (testing "Post a new message for Chris"
    (let [text "{\"message\":\"This is a new message!\"}"
          message (app (mock/content-type
                        (mock/request :post "/api/Chris/publish"
                                      text)
                        "application/json"))]
      (m/is (m/contains-string
             (format "{\"message\":\"This is a new message!\",\"author\":\"Chris\",\"timestamp\":\"%s"
                     (tf/unparse
                      (tf/formatter "dd-MM-yyyy hh:mm")
                      (t/now))))
            (:body message))
      (m/is (m/= 201) (:status message))
      (m/is (m/= "/api/Chris/publish") (get-in message [:headers "Location"])))))
